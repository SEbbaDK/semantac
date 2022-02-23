{-# LANGUAGE OverloadedStrings #-}
module Parser where
    
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

s = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "#*" "*#")

domainNameParser = takeWhile1 (inClass "A-Z")
domainParser = do
    string "domain"
    s
    val <- domainNameParser
    s
    char ':'


doparse text = parse domainParser text

