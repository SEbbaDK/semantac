#!/usr/bin/env runhaskell
{-# LANGUAGE NamedFieldPuns #-}

import           Data.Semigroup        ((<>))
import           Errors                (Error (Error), showErrorMessage,
                                        showStackTrace, showErrorInSource)
import           Options.Applicative
import           Parser                (doParse)
import           Text.Megaparsec.Error (errorBundlePretty)
import           TypeChecker           (check)

data Args
  = Args
    { file  :: String
    , latex :: Bool
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
  src <- readFile file
  case doParse file src of
    Left err ->
      putStrLn $ "Parsing error\n" ++ errorBundlePretty err
    Right ast -> do
      print ast
      putStrLn "\n"
      case check ast  of
        Right _  -> putStrLn "Checks passed"
        Left err -> do
          putStrLn $ "Error: " ++ showErrorMessage err
          putStrLn $ showErrorInSource err src
          putStrLn $ concatMap ("  in " ++) (showStackTrace err)
cli Args {file, latex = True} = do
  putStrLn "latex mode"
