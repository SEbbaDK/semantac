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

ws :: Parser ()
ws =
  L.space
    Text.Megaparsec.Char.space1
    (L.skipLineComment "#")
    (L.skipBlockComment "#*" "*#")

premiseSeparator :: Parser ()
premiseSeparator = label "premise separator" $ do
  try (string "   ") <|> string "\n"
  ws

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
  ws
  initial <- try domainNameParser
  ws
  system <- try systemNameParser
  ws
  final <- try domainNameParser
  return $ System
    { arrow = system
    , initial = Custom initial
    , final = Custom final
    }

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

betweenCharEscaped :: Char -> Parser [Char]
betweenCharEscaped delimiter = do
  char delimiter
  res <- many $ satisfy (\c -> c /= delimiter)
  char delimiter
  return res

baseTypeParser :: Parser Spec
baseTypeParser =
  value (string "Integer") Integer
    <|> value (string "Identifier") Identifier

operSpecParser :: ([Spec] -> Spec) -> Parser a -> Parser Spec
operSpecParser opCon op = do
  let sep = try (ws >> op >> ws)
  xs <- sepBy (try baseTypeParser) sep
  return $ opCon xs

specParser :: Parser Spec
specParser =
  try baseTypeParser
  <|> try (operSpecParser Union (string "U"))
  <|> try (operSpecParser Cross (string "×"))

domainVariableBaseParser = some lowerChar

domainParser :: Parser Domain
domainParser = do
  try (string "domain")
  ws
  variable <- domainVariableBaseParser
  ws
  string "∈" <|> string "in"
  ws
  name <- domainNameParser
  ws
  char ':'
  ws
  spec <- specParser
  return Domain { domain = name, variable, spec }

elemParser :: Parser Conf
elemParser = try elemSyntaxParser <|> elemVarParser
  where
    elemSyntaxParser :: Parser Conf
    elemSyntaxParser = betweenCharEscaped '"' >>= \s -> return $ Syntax s

    elemVarParser :: Parser Conf
    elemVarParser = (some alphaNumChar) >>= \s -> return $ Variable s

comma :: Parser ()
comma = do
  ws
  string ","
  void ws

propertyParser :: Parser Property
propertyParser =
  try deterministic <|> terminating
    where
  deterministic = value (string "non-deterministic") NonDeterministic
  terminating = value (string "non-terminating") NonTerminating

confPartParser =
  elemParser `sepBy` ws >>= \e -> return $ Tup e

confParser :: Parser Conf
confParser =
  betweenS
    "<"
    (fmap Tup (confPartParser `sepBy` comma))
    ">"

transParser :: Parser Trans
transParser = do
  before <- try confParser
  ws
  system <- systemNameParser
  ws
  after <- confParser
  return $ Trans {system, before, after}

premiseParser :: Parser Premise
premiseParser = try (
  do
    t <- transParser
    return $ TPremise t
  ) <|> (
  do
    let e = error "Add premise equality"
    return $ TEquality $ Eq (EVar "x") (EVar "x")
  )

ruleParser :: Parser Rule
ruleParser = do
  properties <- propertyListParser
  ws
  string "rule"
  ws
  name <- domainNameParser
  ws
  premises <- premiseParser `sepBy` premiseSeparator
  ruleSepParser
  base <- transParser
  return $ Rule {name, base, premises, properties}
  where
    ruleSepParser = string "---" >> many (char '-') >> ws
    propertyListParser = many (do p <- propertyParser ; ws ; return p)

topParser :: Parser Top
topParser =
  topParser_ (Top [] [] [])
  where
    topParser_ :: Top -> Parser Top
    topParser_ (Top domains systems rules) =
      value (ws >> eof) (Top domains systems rules)
        <|> try ( do
                ws
                d <- domainParser
                topParser_ $ Top (d : domains) systems rules
            )
        <|> try ( do
                ws
                s <- systemParser
                topParser_ $ Top domains (s : systems) rules
            )
        <|> try ( do
                ws
                r <- ruleParser
                topParser_ $ Top domains systems (r : rules)
            )

doParse :: String -> String -> Either (ParseErrorBundle Text Void) Top
doParse filename contents =
  parse topParser filename (pack contents)
