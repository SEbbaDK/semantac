#!/usr/bin/env runhaskell
{-# LANGUAGE NamedFieldPuns #-}
module Main where
import           Control.Monad         (forM_, when)
import           Data.Map.Strict       (Map, foldlWithKey)
import           Data.Semigroup        ((<>))
import           Errors                (Error (Error), showErrorInSource,
                                        showErrorMessage, showStackTrace)
import           Options.Applicative
import           Parser                (doParse)
import           System.IO             (hPutStr, stderr)
import           Text.Megaparsec.Error (errorBundlePretty)
import           TypeChecker           (typeCheck)
import           Types
import           Binding               (bindCheck)

putErr :: String -> IO ()
putErr = hPutStr stderr

data Args
  = Args
    { file       :: String
    , printast   :: Bool
    , printlatex :: Bool
    , printBinds :: Bool
    }

parser :: Parser Args
parser =
  Args
  <$> argument str (metavar "FILE")
  <*> switch (long "print-ast"   <> short 'a' <> help "Print the AST")
  <*> switch (long "print-latex" <> short 'l' <> help "Print latex output")
  <*> switch (long "print-types" <> short 't' <> help "Print binding types")

main :: IO ()
main = cli =<< execParser (info (parser <**> helper) (fullDesc <> progDesc "test" <> header "test2"))

cli :: Args -> IO ()
cli Args {file, printast, printlatex = False, printBinds} = do
  src <- readFile file
  case doParse file src of
    Left err ->
      putErr $ "Parsing Error: " ++ errorBundlePretty err
    Right ast -> do
      when printast (print ast)
      putStrLn $ case bindCheck ast of
        Nothing -> "No bind errors"
        Just e -> unlines e
      case typeCheck ast of
        Right allBinds  -> do
          putStrLn "Checks passed"
          when printBinds (forM_ allBinds (putStrLn . showBinds))
        Left err -> do
          putErr $ "Type Error: " ++ showErrorMessage src err
          -- putErrLn $ showErrorInSource err src
          putErr $ concatMap ("  in " ++) (showStackTrace err)
cli Args {file, printast, printlatex = True} = do
  putStrLn "latex mode"

showBinds :: Map String Type -> String
showBinds = foldlWithKey (\a name type_ -> a ++ "\n" ++ name ++ " :: " ++ show type_) []
