#!/usr/bin/env runhaskell
{-# LANGUAGE NamedFieldPuns #-}

module Main where
import           Control.Monad         (forM_, when)
import           Data.Map.Strict       (Map, foldlWithKey)
import           Data.Semigroup        ((<>))
import           Data.List             (intercalate)
import           Errors                (Error (Error), showError, bold)
import           Options.Applicative
import           Parser                (doParse)
import           System.IO             (hPutStr, stderr)
import qualified System.Exit as Exit
import           Text.Megaparsec.Error (errorBundlePretty)
import           TypeChecker           (typeCheck, CheckResult)
import           Types
import           Ast                   (Variable, name)
import           Binding               (bindCheck)
import           Pretty

putErr :: String -> IO ()
putErr = hPutStr stderr

data Args
  = Args
    { file        :: String
    , printast    :: Bool
    , printpretty :: Bool
    , printlatex  :: Bool
    , printbinds  :: Bool
    }

parser :: Parser Args
parser =
  Args
  <$> argument str (metavar "FILE")
  <*> switch (long "print-ast"    <> short 'a' <> help "Print the AST")
  <*> switch (long "print-pretty" <> short 'p' <> help "Pretty-print the specification")
  <*> switch (long "print-latex"  <> short 'l' <> help "Print latex output")
  <*> switch (long "print-types"  <> short 't' <> help "Print binding types")

main :: IO Int
main = cli =<< execParser (info (parser <**> helper) (fullDesc <> progDesc "test" <> header "test2"))

-- Error codes:
-- 0 - Everything is fine
-- 1 - Parsing failed
-- 2 - Bind Errors
-- 3 - Type Errors
-- 9 - Both Bind and Type Errors
cli :: Args -> IO Int
cli Args {file, printast, printlatex = True} = do
  putStrLn "latex mode"
  exit 0
cli Args {file, printast, printlatex = False, printpretty, printbinds} = do
  src <- readFile file
  case doParse file src of
    Left err -> do
      putErr $ "Parsing Error: " ++ errorBundlePretty err
      exit 1
    Right ast -> do
      when printast (putStrLn $ show ast)
      when printpretty (putStrLn $ pprint ast)

      hadBindErrors <- case bindCheck ast of
        Nothing -> return False
        Just e -> do
            putStr $ showErrors "Bind Error" src e
            return True

      case typeCheck ast of
        Left err -> do
          putErr $ "\n\n" ++ showErrors "Type Error" src err
          exit $ if hadBindErrors then 9 else 3
        Right checkResults  -> do
          putStrLn "Checks passed"
          when printbinds (putStr $ unlines $ map printCheckResult checkResults)
          exit $ if hadBindErrors then 2 else 0

exit 0 = Exit.exitWith $ Exit.ExitSuccess
exit n = Exit.exitWith $ Exit.ExitFailure n

showErrors errType src errs =
    let
        errList = map (showError src) errs
        errlistHeaders = map ((bold errType ++ ": ") ++) errList
    in
        intercalate "\n\n" errlistHeaders
    

printCheckResult :: CheckResult -> String
printCheckResult (rule, bind) = unlines
    [ "Binds for rule " ++ name rule
    , printBinds bind
    ]

printBinds :: Map Variable Type -> String
printBinds = foldlWithKey (\a x type_ -> a ++ "\n" ++ show x ++ " :: " ++ show type_) []

