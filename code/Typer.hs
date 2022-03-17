{-# LANGUAGE NamedFieldPuns #-}

module Typer where

import           Ast
import           TestAst (testAst)


check :: Top -> Bool
check (Top domains systems rules) =
    foldl
        (\pass rule -> pass && checkRule domains rule)
        True
        rules

checkRule :: [Domain] -> Rule -> Bool
checkRule domains Rule {base, premises, properties} =
    True



main :: IO ()
main = do
    putStrLn "----- AST -----"
    print testAst
    putStrLn "----- Typechecks -----"
    putStr $ (show . check) testAst
    return ()

