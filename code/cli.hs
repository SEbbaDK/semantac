#!/usr/bin/env runhaskell

{-# LANGUAGE NamedFieldPuns #-}

import Data.Bifunctor (second)
import Data.Either (rights)
import Data.Semigroup ((<>))
import Options.Applicative
import Parser (doParse)
import Text.Megaparsec.Error (errorBundlePretty)

data Args = Args
  { file :: String,
    latex :: Bool
  }

parser :: Parser Args
parser =
  Args
    <$> argument str (metavar "FILE")
    <*> switch (long "latex" <> short 'l' <> help "Print latex output")

main :: IO ()
main = cli =<< execParser (info (parser <**> helper) (fullDesc <> progDesc "test" <> header "test2"))

cli :: Args -> IO ()
cli Args {file, latex = False} = do
  content <- readFile file
  case doParse file content of
    Left err ->
      putStrLn $ "Parsing error\n" ++ (errorBundlePretty err)
    Right ast ->
      putStrLn (show ast)
cli Args {file, latex = True} = do
  putStrLn "latex mode"
