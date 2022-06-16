#!/usr/bin/env runhaskell
{-# LANGUAGE NamedFieldPuns #-}

module Main where
import           Control.Monad         (forM_, when)
import           Data.Bits             ((.|.))
import           Data.List             (intercalate)
import           Data.Map.Strict       (Map, toList)
import           Data.Semigroup        ((<>))
import           Options.Applicative
import           System.IO             (hPutStr, stderr)
import           Text.Megaparsec.Error (errorBundlePretty)
import qualified System.Exit as Exit

import           Ast                   (Variable, name, sRules)
import           Binding               (bindCheck)
import           Errors                (Error (Error), showError, bold, showErrorMessage)
import           OverlapChecker
import           Latex
import           Parser                (doParse)
import           Pretty
import           TypeChecker           (typeCheck, CheckResult)
import           Types

putErr :: String -> IO ()
putErr = hPutStr stderr

data Args
  = Args
    { file        :: String
    , printast    :: Bool
    , printpretty :: Bool
    , printlatex  :: Bool
    , printtypes  :: Bool
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
-- 2 - Overlap Errors
-- 4 - Bind Errors
-- 8 - Type Errors
-- other - Bit pattern of the other errors
cli :: Args -> IO Int
cli Args {file, printast, printlatex, printpretty, printtypes} = do
  src <- readFile file
  case doParse file src of
    Left err -> do
      putErr $ "Parsing Error: " ++ errorBundlePretty err
      exit 1
    Right spec -> do
      when printast (putStrLn $ show spec)
      when printpretty (putStrLn $ pprint spec)
      when printlatex (putStrLn $ latexify $ head $ sRules spec)
      overlapExit <- case overlapCheck spec of
        Nothing -> return 0
        Just e -> do
          putStr $ showErrorsWith (showErrorMessage src) src "Overlap Error" e
          return 2
      bindExit <- case bindCheck spec of
        Nothing -> return 0
        Just e -> do
          putStr $ showErrors src "Bind Error" e
          return 4
      case typeCheck spec of
        Left err -> do
          putErr $ "\n\n" ++ showErrors src "Type Error" err
          exit $ foldl (.|.) 8 [ overlapExit, bindExit ]
        Right checkResults -> do
          when printtypes (putStr $ unlines $ map printCheckResult checkResults)
          exit $ foldl (.|.) 0 [ overlapExit, bindExit ]

exit 0 = Exit.exitWith $ Exit.ExitSuccess
exit n = Exit.exitWith $ Exit.ExitFailure n

showErrors src = showErrorsWith (showError src) src
showErrorsWith f src errType errs =
    let
        errList = map f errs
        errlistHeaders = map ((bold errType ++ ": ") ++) errList
    in
        intercalate "\n\n" errlistHeaders

printCheckResult :: CheckResult -> String
printCheckResult (rule, bind) = unlines $
    ("Binds for rule " ++ (bold $ name rule)) : (map format $ toList bind)
    where
        format (v,t) = "    " ++ (bold $ pprint v) ++ " :: " ++ pprint t

