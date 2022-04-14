#!/usr/bin/env runhaskell
{-# LANGUAGE NamedFieldPuns #-}
module Main where
import           Data.Semigroup        ((<>))
import           Errors                (Error (Error), showErrorMessage,
                                        showStackTrace, showErrorInSource)
import           Options.Applicative
import           Parser                (doParse)
import           Text.Megaparsec.Error (errorBundlePretty)
import           TypeChecker           (check)
import           System.IO             (stderr, hPutStr)

putErr = hPutStr stderr

data Args
  = Args
    { file       :: String
    , printast   :: Bool
    , printlatex :: Bool
    }

parser :: Parser Args
parser =
  Args
  <$> argument str (metavar "FILE")
  <*> switch (long "print-ast"   <> short 'a' <> help "Print the AST")
  <*> switch (long "print-latex" <> short 'l' <> help "Print latex output")

main :: IO ()
main = cli =<< execParser (info (parser <**> helper) (fullDesc <> progDesc "test" <> header "test2"))

cli :: Args -> IO ()
cli Args {file, printast, printlatex = False} = do
  src <- readFile file
  case doParse file src of
    Left err ->
      putErr $ "Parsing Error: " ++ errorBundlePretty err
    Right ast -> do
      if printast then print ast else return ()
      case check ast of
        Right _  -> putStrLn "Checks passed"
        Left err -> do
          putErr $ "Type Error: " ++ showErrorMessage src err
          -- putErrLn $ showErrorInSource err src
          putErr $ concatMap ("  in " ++) (showStackTrace err)
cli Args {file, printast, printlatex = True} = do
  putStrLn "latex mode"
