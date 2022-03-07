{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad (void)
import Data.Text (Text, pack, unpack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Parsec (many1)

import Ast

type Parser = Parsec Void Text

s :: Parser ()
s = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "#*" "*#")

domainNameParser :: Parser [Char]
domainNameParser =
  do
    c <- try upperChar
    rest <- many letterChar
    return $ c : rest

value parser result =
  do
    _ <- try parser
    return result

baseTypeParser =
  value (string "Integer") Integer <|>
  value (string "Identifier") Identifier

operSpecParser op opCon =
  do
    l <- try baseTypeParser
    _ <- try op
    r <- try specParser
    return opCon [l,r]

specParser =
  try (baseTypeParser) <|>
  try (operSpecParser (string "U") (\x -> Union x)) <|>
  try (operSpecParser (string "×") (\x -> Cross x))

domainParser :: Parser Domain
domainParser = do
  string "domain"
  s
  name <- domainNameParser
  s
  string "="
  s
  spec <- specParser
  return Domain name spec

--doParse :: String -> String -> Either (ParseErrorBundle Text Void) String
doParse filename contents = parse domainParser filename (pack contents)

