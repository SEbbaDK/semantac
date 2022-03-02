{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Parsec (many1)

type Parser = Parsec Void Text

s :: Parser ()
s = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "#*" "*#")

domainNameCharacters :: [Char]
domainNameCharacters = "ABCDEFGHJKLMNOPQRSTUVWXYZ"

domainNameParser :: Parser [Char]
domainNameParser =
  do
    c <- try (oneOf domainNameCharacters)
    rest <- many (oneOf domainNameCharacters)
    return $ c : rest

domainParser :: Parser String
domainParser = do
  string "domain"
  s
  val <- domainNameParser
  s
  string ":"
  return val

doParse :: Text -> Either (ParseErrorBundle Text Void) String
doParse = parse domainParser "TODO: name of source file"
