#!/usr/bin/env runhaskell

import Options.Applicative
import Data.Semigroup ((<>))

import Parser

data Args = Args
    { file  :: String
    , latex :: Bool
    }

parser = Args
    <$> argument str (metavar "FILE")
    <*> switch (long "latex" <> short 'l' <> help "Print latex output")

main = cli =<< execParser (info (parser <**> helper) ( fullDesc <> progDesc "test" <> header "test2" ))

cli :: Args -> IO ()
cli (Args file False) = do
    content <- readFile file
    let parsed = doparse content
    putStrLn parsed

cli (Args file True) = do
    putStrLn "latex mode"

