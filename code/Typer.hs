{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Typer where

import           Ast
import           TestAst (testAst)
import Language.Haskell.TH (match)

{-

What can we typecheck?
- Transitions are consistent with their systems.
- Variables in the conclusion must be defined.
- Variables must not conflict with eachother.
- Syntax nodes must be consistent with the grammar.

In order to do any of this, there needs to be a grammar defined.


-}


check :: Top -> Bool
check (Top domains systems rules) =
    foldl
        (\pass rule -> pass && checkRule domains systems rule)
        True
        rules

checkRule :: [Domain] -> [System] -> Rule -> Bool
checkRule domains systems Rule {base, premises, properties} =
    premisesOk
    where
        premisesOk = all (checkPremiseSystems systems) premises

-- Premise matches system
checkPremiseSystems :: [System] -> Premise -> Bool
checkPremiseSystems systems (TPremise trans) = any (\sys -> checkTransSystem sys trans) systems
checkPremiseSystems _ _ = True

-- Transition matches system
checkTransSystem :: System -> Trans -> Bool
checkTransSystem System { arrow, initial, final } Trans { system, before, after } =
    initial == beforeSpec && final == afterSpec
    where
        beforeSpec = specOfConf before
        afterSpec = specOfConf after

specOfConf :: Conf -> Spec
specOfConf (Syntax _) = SSyntax
specOfConf (Variable x) = Identifier
specOfConf (Tup xs) = Cross (fmap specOfConf xs)


main :: IO ()
main = do
    putStrLn "----- AST -----"
    putStrLn ""
    print testAst
    putStrLn ""
    putStrLn "----- Typechecks -----"
    putStr $ (show . check) testAst
    return ()

