#!/usr/bin/env runhaskell

import Options.Applicative
import Data.Semigroup ((<>))

data Args = Args
    { latex :: Bool
    }

parser = Args
    <$> switch (long "latex" <> short 'l' <> help "Print latex output")

main = cli =<< execParser (info (parser <**> helper) ( fullDesc <> progDesc "test" <> header "test2" ))

cli :: Args -> IO ()
cli (Args False) = do
    putStrLn "not latex"

cli (Args True) = do
    putStrLn "latex mode"

