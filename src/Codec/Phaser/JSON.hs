{-# LANGUAGE ApplicativeDo, ScopedTypeVariables #-}
module Codec.Phaser.JSON (
  JSONValue(..),
  jsonDocument
 ) where

import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import Codec.Phaser
import Codec.Phaser.Core
import Codec.Phaser.UTF16
import Data.Bits
import Data.Char
import Data.Word
import Numeric

data JSONValue =
  JSONString T.Text |
  JSONNumber Rational |
  JSONObject [(T.Text, JSONValue)] |
  JSONArray [JSONValue] |
  JSONBool Bool |
  JSONNull

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
jsonValue = undefined
  -- I'm still figuring out why this type annotation is necessary
  (JSONString <$> (jsonString :: Phase p Char o T.Text)) <|>
  (JSONNumber <$> scientificNotation) <|>
  jsonObject <|>
  jsonArray <|>
  (JSONBool <$> jsonBool) <|>
  (JSONNull <$ string "null")

instance Show JSONValue where
  showsPrec p v = go v where
    go (JSONString s) = ('\"':) . T.foldr (\c r -> case c of
      '\\' -> ("\\\\" ++) . r
      '\"' -> ("\\\"" ++) . r
      _ -> (c :) . r
     ) ('\"':) s
