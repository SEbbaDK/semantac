{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Parser where

import           Ast
import           Data.Text                  (Text, pack)
import           Data.Void                  (Void)
import           Loc
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Pos        (unPos)
import           Types

type Parser = Parsec Void Text

pos2tup :: SourcePos -> (FilePath, Int, Int)
pos2tup SourcePos {sourceName, sourceLine, sourceColumn} =
  (sourceName, unPos sourceLine, unPos sourceColumn)

getPos :: Parser (FilePath, Int, Int)
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

systemNameParser :: Parser String
systemNameParser =
  many (oneOf systemNameChars <|> alphaNumChar)
  where systemNameChars :: [Char]
        systemNameChars = "-=~|>→↓#"

systemParser :: Parser SystemDecl
systemParser = do
  try (string "system")
  ws
  initial <- locced typeParser
  ws
  system <- systemNameParser
  ws
  final <- locced typeParser
  return $ SystemDecl
    { arrow = system
    , initial = initial
    , final = final
    }

value :: Parser a -> b -> Parser b
value parser result = try parser >> return result

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

baseTypeParser :: Parser Type
baseTypeParser =
  between (string "(" >> ws) (ws >> ")") typeParser
    <|> fmap TPrimitive (betweenCharEscaped '"')
    <|> fmap TAlias categoryNameParser

wtry :: Parser a -> Parser a
wtry p = try $ ws >> p

typeParser :: Parser Type
typeParser = let
  biToArray f l r = f [l,r]
  unionParse = value (string "U" <|> string "∪") (Just $ biToArray TUnion)
  crossParse = value (string "x" <|> string "×" <|> string "⨯") (Just $ biToArray TCross)
  funcParse = value (string "->") (Just TFunc)
 in do
  l <- baseTypeParser
  o <- wtry unionParse <|> wtry crossParse <|> wtry funcParse <|> return Nothing
  case o of
    Nothing -> return l
    Just f  -> fmap (f l) typeParser

functionNameParser :: Parser String
functionNameParser = many alphaNumChar

declarationParser :: Parser TermDecl
declarationParser = do
  try (string "meta")
  ws
  name <- functionNameParser
  ws
  char '='
  ws
  spec <- typeParser
  ws
  return $ TermDecl name spec

aliasParser :: Parser CategoryDecl
aliasParser = do
  try (string "alias")
  ws
  name <- categoryNameParser
  ws
  isIn <- value (try (char '=')) False <|> value (char '∈') True
  ws
  type_ <- typeParser
  ws
  return $ CategoryDecl name type_ isIn




propertyParser :: Parser Property
propertyParser =
  try deterministic <|> terminating
    where
      deterministic = value (string "non-deterministic") NonDeterministic
      terminating = value (string "non-terminating") NonTerminating

bindingParser :: Parser (Variable, Variable)
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
  marks <- many (char '\'')
  binds <- many bindingParser
  return $ Variable base (name ++ marks) binds

confElementParser :: Parser (Loc Conf)
confElementParser = do
  start <- getPos
  let parsers = try elemSyntaxParser <|> try elemVarParser <|> elemParenParser
  elems <- locced parsers `sepBy` ws
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
    where comma = ws >> string "," >> ws

transParser :: Parser Transition
transParser = do
  before <- try $ locced confParser
  ws
  system <- systemNameParser
  ws
  after <- locced confParser
  return $ Transition {system, before, after}

exprParamParser :: Parser [Expr]
exprParamParser = betweenS "(" (exprParser `sepBy` some (string "," >> ws)) ")"

exprParser :: Parser Expr
exprParser = do
  lhs <- functionNameParser
  params <- exprParamParser
  ws
  return $ ECall lhs params

eqParser :: Parser Equality
eqParser = do
  left <- exprParser
  ws
  eq <- try (value (string "==") Eq) <|> value (string "!=") InEq
  ws
  right <- exprParser
  ws
  return $ eq left right

premiseParser :: Parser Premise
premiseParser = do
  p <- fmap PTransition transParser <|> fmap PEquality eqParser
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

topParser :: Parser Specification
topParser = do
  spec <- topParser_ (Specification [] [] [] [])
  return Specification
      { sTerms = reverse (sTerms spec)
      , sCategories = reverse (sCategories spec)
      , sSystems = reverse (sSystems spec)
      , sRules = reverse (sRules spec)
      }
  where
    topParser_ :: Specification -> Parser Specification
    topParser_ spec = ws >>
      value eof spec
        <|> try ( do
                d <- locced declarationParser
                topParser_ $ spec { sTerms = d : sTerms spec }
            )
        <|> try ( do
                a <- locced aliasParser
                topParser_ $ spec { sCategories = a : sCategories spec }
            )
        <|> try ( do
                s <- locced systemParser
                topParser_ $ spec { sSystems = s : sSystems spec }
            )
        <|> try ( do
                r <- locced ruleParser
                topParser_ $ spec { sRules = r : sRules spec }
            )

doParse :: String -> String -> Either (ParseErrorBundle Text Void) Specification
doParse filename contents =
  parse topParser filename (pack contents)
