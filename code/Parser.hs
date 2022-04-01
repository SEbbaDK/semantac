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
import           Text.Megaparsec.Pos        (unPos)

type Parser = Parsec Void Text

pos2tup (SourcePos {sourceName, sourceLine, sourceColumn}) =
  (sourceName, unPos sourceLine, unPos sourceColumn)

locced :: Parser a -> Parser (Loc a)
locced p = do
  start <- fmap pos2tup getSourcePos
  result <- p
  end <- fmap pos2tup getSourcePos
  return $ Loc (start, end) result

ws :: Parser ()
ws =
  L.space
    Text.Megaparsec.Char.space1
    (L.skipLineComment "#")
    (L.skipBlockComment "#*" "*#")

varNameParser :: Parser (String, String, Int)
varNameParser = do
  base <- some alphaNumChar
  name <- option "" $ char '_' >> some alphaNumChar
  marks <- fmap length $ many (char '\'')
  return (base, name, marks)

categoryNameParser :: Parser String
categoryNameParser = do
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

operSpecParser :: ([Spec] -> Spec) -> Parser a -> Parser Spec
operSpecParser opCon op = do
  let sep = try (ws >> op >> ws)
  xs <- (try baseSpecParser) `sepBy` sep
  return $ opCon xs

baseSpecParser :: Parser Spec
baseSpecParser =
  (between (string "(" >> ws) (ws >> ")" >> ws) specParser)
    <|> value (string "Integer") Integer
    <|> value (string "Identifier") Identifier
    <|> value (string "Syntax") SSyntax
    <|> fmap Custom categoryNameParser

specParser :: Parser Spec
specParser = let
  biToArray f l r = f [l,r]
  unionParse = value (string "U" <|> string "∪") (Just $ biToArray Union)
  crossParse = value (string "x" <|> string "×" <|> string "⨯") (Just $ biToArray Cross)
  funcParse = value (string "->") (Just $ Func)
 in do
  l <- baseSpecParser
  ws
  o <- try unionParse <|> try crossParse <|> try funcParse <|> return Nothing
  ws
  case o of
    Nothing -> return l
    Just f -> fmap (f l) baseSpecParser

categoryVariableBaseParser = some lowerChar

categoryParser :: Parser Category
categoryParser = do
  try (string "category")
  ws
  variable <- categoryVariableBaseParser
  ws
  string "∈" <|> string "in"
  ws
  name <- categoryNameParser
  ws
  char '='
  ws
  spec <- specParser
  ws
  return $ Category { category = name, variable, spec }

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

confElementParser :: Parser Conf
confElementParser = do
  let parsers = try elemSyntaxParser <|> try elemVarParser <|> elemParenParser
  elems <- parsers `sepBy` ws
  return $ case elems of
    [e] -> e
    l   -> SyntaxList l

  where
    elemSyntaxParser :: Parser Conf
    elemSyntaxParser = fmap Syntax $ betweenCharEscaped '"'
  
    elemVarParser :: Parser Conf
    elemVarParser    = fmap (\(x,s,m) -> Variable x s m) varNameParser
  
    elemParenParser :: Parser Conf
    elemParenParser  = fmap Paren $ betweenS "(" confElementParser ")"
  
confParser :: Parser Conf
confParser =
  betweenS
    "<"
    (fmap Conf (confElementParser `sepBy` comma))
    ">"
      where

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
  name <- categoryNameParser
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
    topParser_ (Top categories systems rules) = ws >>
      value eof (Top (reverse categories) (reverse systems) (reverse rules))
        <|> try ( do
                c <- locced categoryParser
                topParser_ $ Top (c : categories) systems rules
            )
        <|> try ( do
                s <- locced systemParser
                topParser_ $ Top categories (s : systems) rules
            )
        <|> try ( do
                r <- locced ruleParser
                topParser_ $ Top categories systems (r : rules)
            )

doParse :: String -> String -> Either (ParseErrorBundle Text Void) Top
doParse filename contents =
  parse topParser filename (pack contents)
