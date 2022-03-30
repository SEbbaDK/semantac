{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

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

varNameParser :: Parser String
varNameParser = do
  start <- some alphaNumChar
  end <- try (fmap unpack (string "'")) <|> return ""
  return $ start ++ end

domainNameParser :: Parser String
domainNameParser = do
  c <- try upperChar
  rest <- many letterChar
  return $ c : rest

systemNameChars :: [Char]
systemNameChars = "-=~|>#abcdefghjklmnop"

systemNameParser :: Parser String
systemNameParser = many (oneOf systemNameChars)

systemParser :: Parser System
systemParser = do
  try (string "system")
  ws
  initial <- specParser
  ws
  system <- systemNameParser
  ws
  final <- specParser
  return $ System
    { arrow = system
    , initial = initial
    , final = final
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
    <|> value (string "Syntax") SSyntax
    <|> (do name <- domainNameParser ; return $ Custom name)

operSpecParser :: ([Spec] -> Spec) -> Parser a -> Parser Spec
operSpecParser opCon op = do
  let sep = try (ws >> op >> ws)
  xs <- (try baseTypeParser) `sepBy` sep
  return $ opCon xs

specParser :: Parser Spec
specParser =
  try baseTypeParser
  <|> try (operSpecParser Union (string "U" <|> string "∪"))
  <|> try (operSpecParser Cross (string "x" <|> string "×" <|> string "⨯"))
  <|> try (do a <- baseTypeParser; ws >> (string "->") >> ws; b <- baseTypeParser; return $ Func a b)

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
    elemSyntaxParser = fmap Syntax (betweenCharEscaped '"')

    elemVarParser :: Parser Conf
    elemVarParser = fmap Variable varNameParser

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
  elemParser `sepBy` ws >>= \e -> return $ SupTup e

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

exprVarParser :: Parser Expr
exprVarParser = do
  s <- some alphaNumChar
  return $ EVar s

exprCallParser :: Parser Expr
exprCallParser = do
  name <- (do string "+"; n <- some lowerChar; return ("+" ++ n))
  ws
  params <- exprVarParser `sepBy` (some $ string " ")
  return $ EOp name params

exprParser :: Parser Expr
exprParser = try (exprVarParser) <|> exprCallParser

eqParser :: Parser Equality
eqParser = do
  try $ string "["
  ws
  left <- exprParser
  ws
  eq <- try (value (string "==") Eq) <|> (value (string "!=") InEq)
  ws
  right <- exprParser
  ws
  string "]"
  return $ eq left right

premiseParser :: Parser Premise
premiseParser = do
  p <- fmap TPremise transParser <|> fmap TEquality eqParser
  ws
  return p

ruleParser :: Parser Rule
ruleParser = do
  properties <- propertyListParser
  ws
  string "rule"
  ws
  name <- domainNameParser
  ws
  premises <- many premiseParser
  ws
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
      value (ws >> eof) (Top (reverse domains) (reverse systems) (reverse rules))
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
