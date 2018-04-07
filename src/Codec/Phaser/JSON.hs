{-# LANGUAGE ApplicativeDo, ScopedTypeVariables #-}
module Codec.Phaser.JSON (
  JSONValue(..),
  Precision(..),
  jsonDocument,
  jsonValue,
  showJSON,
 ) where

import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import Codec.Phaser
import Codec.Phaser.Core
import Codec.Phaser.UTF16
import Data.Bits
import Data.Char
import Data.List (genericLength)
import Data.Semigroup
import Data.Word
import Numeric

data JSONValue =
  JSONString T.Text |
  JSONNumber Rational |
  JSONObject [(T.Text, JSONValue)] |
  JSONArray [JSONValue] |
  JSONBool Bool |
  JSONNull

data Precision = SF Integer | DP Integer | SFDP Integer Integer

instance Show JSONValue where
  showsPrec _ v = case parse_ () (showJSON v (SFDP 18 18) >># b id) [] of
    Right (s:_) -> s
   where
    b acc = (<|> return acc) $ do
      c <- get
      let acc' = acc . (c:)
      b acc'

jsonString :: Monoid p => Phase p Char o T.Text
jsonString = T.pack <$> (char '\"' *> many c <* char '\"') where
  c = satisfy ((&&) <$> (/= '\"') <*> (/= '\\')) <|> (char '\\' *> bs)
  bs = get >>= \x -> case x of
    '\"' -> return '\"'
    '\\' -> return '\\'
    '/' -> return '/'
    'b' -> return '\b'
    'f' -> return '\f'
    'n' -> return '\n'
    'r' -> return '\r'
    't' -> return '\t'
    'u' -> fromAutomaton $ sepBy (h4 >>= yield) (string "\\u") >># utf16_char 
  h4 = (\a b c d -> shiftL a 12 .|. shiftL b 8 .|. shiftL c 4 .|. d) <$>
    h <*> h <*> h <*> h
  h = (fromIntegral . digitToInt) <$> satisfy isHexDigit

jsonObject :: Monoid p => Phase p Char o JSONValue
jsonObject = do
  char '{'
  munch isSpace
  r <- sepBy (do
    l <- jsonString
    munch isSpace
    char ':'
    munch isSpace
    v <- jsonValue
    munch isSpace
    return (l,v)
   ) (char ',' <* munch isSpace)
  char '}'
  return $ JSONObject r

jsonArray :: Monoid p => Phase p Char o JSONValue
jsonArray = do
  char '['
  munch isSpace
  r <- sepBy (jsonValue <* munch isSpace) (char ',' <* munch isSpace)
  char ']'
  return $ JSONArray r

jsonDocument :: Monoid p => Phase p Char o JSONValue
jsonDocument = do
  munch isSpace
  r <- jsonObject <|> jsonArray
  munch isSpace
  return r

jsonBool :: Monoid p => Phase p Char o Bool
jsonBool = (True <$ string "true") <|> (False <$ string "false")

jsonValue :: forall p o . Monoid p => Phase p Char o JSONValue
jsonValue =
  -- I'm still figuring out why this type annotation is necessary
  (JSONString <$> (jsonString :: Phase p Char o T.Text)) <|>
  (JSONNumber <$> scientificNotation) <|>
  jsonObject <|>
  jsonArray <|>
  (JSONBool <$> jsonBool) <|>
  (JSONNull <$ string "null")

instance Semigroup Precision where
  SF a <> SF b = SF (max a b)
  DP a <> DP b = DP (max a b)
  SF a <> DP b = SFDP a b
  SFDP s d <> SF b = SFDP (max s b) d
  SFDP s d <> DP b = SFDP s (max d b)
  a <> b = b <> a

showJSON :: Monoid p => JSONValue -> Precision -> Phase p i Char ()
showJSON (JSONString str) _ = yield '\"' >>
  foldr (>>) (return ()) (map (\c -> case c of
    '\"' -> (mapM_ yield ("\\\"" :: String))
    '\\' -> (mapM_ yield ("\\\\" :: String))
    '\b' -> (mapM_ yield ("\\b" :: String))
    '\f' -> (mapM_ yield ("\\f" :: String))
    '\n' -> (mapM_ yield ("\\n" :: String))
    '\r' -> (mapM_ yield ("\\r" :: String))
    '\t' -> (mapM_ yield ("\\t" :: String))
    _ | fromEnum c >= 0x20 && fromEnum c <= 0x7E -> yield c
      | otherwise -> let
       step1 = utf16_encode_char c
       hexp cp = let
         offsets = [0x0C, 0x08, 0x04, 0]
         in mapM_ (
           yield .
           toUpper .
           intToDigit .
           fromIntegral .
           (.&. 0x0F) .
           shiftR cp
          ) offsets
       loop = (<|> return ()) $ get >>= \cp ->
         mapM_ yield ("\\u" :: String) >> hexp cp >> loop
       in fromAutomaton $ step1 >># loop
  ) $ T.unpack str) >>
  yield '\"'
showJSON (JSONNumber n) prec = mapM_ yield $ showNumber n prec [] 
showJSON (JSONObject obj) prec = do
  yield '{'
  let
    p (k,v) = do
      showJSON (JSONString k) prec
      yield ':'
      showJSON v prec
    w [] = return ()
    w [kv] = p kv
    w (kv:r) = do
      p kv
      yield ','
      w r
  w obj
  yield '}'
showJSON (JSONArray l) prec = do
  yield '['
  let
    w [] = return ()
    w [v] = showJSON v prec
    w (v:r) = do
      showJSON v prec
      yield ','
      w r
  w l
  yield ']'
showJSON (JSONBool True) _ = mapM_ yield ("true" :: String)
showJSON (JSONBool False) _ = mapM_ yield ("false" :: String)
showJSON JSONNull _ = mapM_ yield ("null" :: String)

showNumber :: Rational -> Precision -> ShowS
showNumber 0 _ = ('0':)
showNumber n p = let
  a' = abs n
  (a,e) = let
    grow a1 e1 = if a1 < 1
      then grow (a1 * 10) (e1 - 1)
      else (a1,e1)
    shrink a1 e1 = if a1 >= 10
      then shrink (a1 / 10) (e1 + 1)
      else (a1,e1)
    in uncurry shrink $ grow a' 0
  rounded = let
    m = 10 ^ p'
    in fromIntegral (round (a * m)) / m
  p' = case p of
    SF x -> x - 1
    DP x -> x + e
    SFDP sf dp -> max (sf - 1) (dp + e)
  (a2,e2) = case e of
    -1 -> (rounded / 10, 0)
    _ -> (rounded, e)
  (dp,de) = if e2 < 0
    then (1,e2)
    else (e2 + 1,0)
  goL 0 0 = id
  goL n' 0 = ('.':) . goR n'
  goL n' dp' = let
    dv = floor n'
    r = (n' - fromIntegral dv) * 10
    in (intToDigit dv:) . goL r (dp' - 1)
  goR 0 = id
  goR n' = let
    dv = floor n'
    r = (n' - fromIntegral dv) * 10
    in (intToDigit dv:) . goR r
  goE = case de of
    0 -> id
    _ -> ('e':) . showsPrec 0 de
  sign = if n < 0 then ('-':) else id
  in sign . goL a2 dp . goE
