{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Parser where

import           Ast
import           Loc
import           Control.Monad              (void)
import           Data.Text                  (Text, pack, unpack)
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Pos        (unPos)

type Parser = Parsec Void Text

pos2tup SourcePos {sourceName, sourceLine, sourceColumn} =
  (sourceName, unPos sourceLine, unPos sourceColumn)

getPos = fmap pos2tup getSourcePos

locced :: Parser a -> Parser (Loc a)
locced p = do
  start <- getPos
  result <- p
  end <- getPos
  return $ Loc (start, end) result

ws :: Parser ()
ws =
  L.space
    Text.Megaparsec.Char.space1
    (L.skipLineComment "#")
    (L.skipBlockComment "#*" "*#")

categoryNameParser :: Parser String
categoryNameParser = do
  c <- try upperChar
  rest <- many letterChar
  return $ c : rest

systemNameChars :: [Char]
systemNameChars = "-=~|>→↓#"

systemNameParser :: Parser String
systemNameParser = many (oneOf systemNameChars <|> alphaNumChar)

systemParser :: Parser System
systemParser = do
  try (string "system")
  ws
  initial <- locced specParser
  ws
  system <- systemNameParser
  ws
  final <- locced specParser
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
    Just f -> fmap (f l) specParser

declarationParser :: Parser Declaration
declarationParser = do
  try (string "meta")
  ws
  name <- many alphaNumChar
  ws
  char '='
  ws
  spec <- specParser
  ws
  return $ Declaration name spec

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

bindingParser = do
  char '['
  var <- variableParser
  ws
  string "|->" <|> string "↦"
  ws
  val <- variableParser
  ws
  char ']'
  return ( var, val )

variableParser :: Parser Variable
variableParser = do
  base <- some alphaNumChar
  name <- option "" $ char '_' >> some alphaNumChar
  marks <- fmap length $ many (char '\'')
  binds <- many bindingParser
  return $ Variable base name marks binds

confElementParser :: Parser (Loc Conf)
confElementParser = do
  start <- getPos
  let parsers = try elemSyntaxParser <|> try elemVarParser <|> elemParenParser
  elems <- (locced parsers) `sepBy` ws
  end <- getPos
  return $ case elems of
    [e] -> e
    l   -> Loc (start,end) $ SyntaxList l

  where
    elemSyntaxParser :: Parser Conf
    elemSyntaxParser = Syntax <$> betweenCharEscaped '"'
  
    elemVarParser :: Parser Conf
    elemVarParser    = Var <$> variableParser
  
    elemParenParser :: Parser Conf
    elemParenParser  = Paren <$> betweenS "(" confElementParser ")"
  
confParser :: Parser Conf
confParser =
  betweenS
    "<"
    (fmap Conf (confElementParser `sepBy` comma))
    ">"

transParser :: Parser Trans
transParser = do
  before <- try $ locced confParser
  ws
  system <- systemNameParser
  ws
  after <- locced confParser
  return $ Trans {system, before, after}

exprParamParser = betweenS "(" (exprParser `sepBy` some (string "," >> ws)) ")"

exprParser :: Parser Expr
exprParser = do
  lhs <- variableParser
  params <- many exprParamParser
  ws
  return $ foldl callify (EVar lhs) params
    where callify base param = ECall base param

eqParser :: Parser Equality
eqParser = do
  left <- exprParser
  ws
  eq <- try (value (string "==") Eq) <|> (value (string "!=") InEq)
  ws
  right <- exprParser
  ws
  return $ eq left right

premiseParser :: Parser Premise
premiseParser = do
  p <- fmap TPremise transParser <|> fmap EPremise eqParser
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
  topParser_ (Top [] [] [] [])
  where
    topParser_ :: Top -> Parser Top
    topParser_ (Top declarations categories systems rules) = ws >>
      value eof (Top (reverse declarations) (reverse categories) (reverse systems) (reverse rules))
        <|> try ( do
                d <- locced declarationParser
                topParser_ $ Top (d : declarations) categories systems rules
            )
        <|> try ( do
                c <- locced categoryParser
                topParser_ $ Top declarations (c : categories) systems rules
            )
        <|> try ( do
                s <- locced systemParser
                topParser_ $ Top declarations categories (s : systems) rules
            )
        <|> try ( do
                r <- locced ruleParser
                topParser_ $ Top declarations categories systems (r : rules)
            )

doParse :: String -> String -> Either (ParseErrorBundle Text Void) Top
doParse filename contents =
  parse topParser filename (pack contents)
