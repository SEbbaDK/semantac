{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Ast
import           Control.Monad              (void)
import           Data.Text                  (Text, pack, unpack)
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

s :: Parser ()
s =
  L.space
    Text.Megaparsec.Char.space1
    (L.skipLineComment "#")
    (L.skipBlockComment "#*" "*#")

lf :: Parser ()
lf = s

domainNameParser :: Parser String
domainNameParser = do
  c <- try upperChar
  rest <- many letterChar
  return $ c : rest

systemNameChars :: [Char]
systemNameChars = "-=~|>#abcdefghjklmnop"

systemNameParser :: Parser String
systemNameParser = many (oneOf systemNameChars)

systemParser = do
  try (string "system")
  s
  initial <- try domainNameParser
  s
  system <- try systemNameParser
  s
  final <- try domainNameParser
  return $ System { arrow = system, initial, final }

value :: Parser a -> b -> Parser b
value parser result = do
  try parser
  return result

betweenS :: Text -> Parser a -> Text -> Parser a
betweenS left middle right = do
  string left
  res <- middle
  string right
  return res

baseTypeParser :: Parser Spec
baseTypeParser =
  value (string "Integer") Integer
    <|> value (string "Identifier") Identifier

operSpecParser :: (Spec -> Spec -> Spec) -> Parser a -> Parser Spec
operSpecParser opCon op = do
  l <- try baseTypeParser
  s
  try op
  s
  r <- try specParser
  return $ opCon l r

specParser :: Parser Spec
specParser =
  try baseTypeParser
  <|> try (operSpecParser Union (string "U"))
  <|> try (operSpecParser Cross (string "×"))

domainParser :: Parser Domain
domainParser = do
  try (string "domain")
  s
  name <- domainNameParser
  s
  string "="
  s
  Domain name <$> specParser

elemParser :: Parser Elem
elemParser = elemVarParser <|> elemSyntaxParser
  where
    elemSyntaxParser :: Parser Elem
    elemSyntaxParser = error "todo"

    elemVarParser :: Parser Elem
    elemVarParser = error "todo"

comma :: Parser ()
comma = do
  s
  string ","
  void s

propertyParser :: Parser Property
propertyParser =
  try deterministic <|> terminating
    where
  deterministic = value (string "non-deterministic") NonDeterministic
  terminating = value (string "non-terminating") NonTerminating

confParser :: Parser Conf
confParser =
  betweenS
    "<"
    (fmap Conf (sepBy1 elemParser comma))
    ">"

transParser :: Parser Trans
transParser = do
  try (string "system")
  s
  before <- confParser
  s
  system <- systemNameParser
  s
  after <- confParser
  return $ Trans {system, before, after}

premiseParser :: Parser Premise
premiseParser = error "todo"

ruleParser :: Parser Rule
ruleParser = do
  properties <- propertyListParser
  s
  string "rule"
  s
  name <- domainNameParser
  lf
  premises <- sepBy premiseParser lf
  ruleSepParser
  base <- transParser
  return $ Rule {name, base, premises, properties}
  where
    ruleSepParser = string "---" >> many (char '-')
    propertyListParser = many (do p <- propertyParser ; s ; return p)

topParser :: Parser Top
topParser =
  topParser_ (Top [] [] [])
  where
    topParser_ :: Top -> Parser Top
    topParser_ (Top domains systems rules) =
      value eof (Top domains systems rules)
        <|> try ( do
                lf
                d <- domainParser
                topParser_ $ Top (d : domains) systems rules
            )
        <|> try ( do
                lf
                s <- systemParser
                topParser_ $ Top domains (s : systems) rules
            )
        <|> ( do
                lf
                r <- ruleParser
                topParser_ $ Top domains systems (r : rules)
            )

doParse :: String -> String -> Either (ParseErrorBundle Text Void) Top
doParse filename contents =
  parse topParser filename (pack contents)
