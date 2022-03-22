{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Typer where

import           Ast
import           Debug.Trace         (trace)
import           Language.Haskell.TH (match)
import           TestAst             (testAst)

{-

What can we check?
- Transitions are consistent with their systems.
- Variables in the conclusion must be defined.
- Variables must not conflict with eachother.
- Syntax nodes must be consistent with the grammar.

In order to do any of this, there needs to be a grammar defined.


-}


data CheckResult
  = Ok
  | SpecMismatch Conf Spec

instance Show CheckResult where
    show Ok                 = "Ok"
    show (SpecMismatch c s) = "Spec mismatch for \"" ++ show c ++ "\", expected " ++ show s ++ " but found " ++ show (specOfConf c)

checkResults :: (a -> CheckResult) -> [a] -> CheckResult
checkResults f = foldl check_ Ok
    where
        check_ Ok x = f x
        check_ r x  = r


check :: Top -> CheckResult
check (Top domains systems rules) =
    checkResults (checkRule domains systems) rules

checkRule :: [Domain] -> [System] -> Rule -> CheckResult
checkRule domains systems Rule {base, premises, properties} =
    checkResults (checkPremiseSystems systems) premises

-- Premise matches system
checkPremiseSystems :: [System] -> Premise -> CheckResult
checkPremiseSystems systems (TPremise trans) = checkResults (\sys -> checkTransSystem sys trans) systems
checkPremiseSystems _ _ = Ok

-- Transition matches system
checkTransSystem :: System -> Trans -> CheckResult
checkTransSystem System { arrow, initial, final } Trans { system, before, after } =
    case (matches before initial, matches after final) of
        (Ok, Ok)              -> Ok
        (SpecMismatch c s, _) -> SpecMismatch c s
        (_, SpecMismatch c s) -> SpecMismatch c s


matches :: Conf -> Spec -> CheckResult
matches c s = case (c, s) of
    (c, Cross [s]) -> matches c s
    (c, Union [s]) -> matches c s
    (Tup [c], s) -> matches c s
    (Syntax c, SSyntax) -> Ok
    (Variable c, Identifier) -> Ok
    (Variable c1, Custom c2) ->
        -- TODO: c1 is simply the name of the identifier, so it actually needs to be looked up in the domains list
        if c1 == c2 then
            Ok
        else
            SpecMismatch c s
    (Tup (c : cs), Cross (s : ss)) ->
        case matches c s of
            Ok -> matches (Tup cs) (Cross ss)
            _  -> SpecMismatch c s
    (_, _) -> SpecMismatch c s


specOfConf :: Conf -> Spec
specOfConf (Syntax _)   = SSyntax
specOfConf (Variable x) = Identifier
specOfConf (Tup xs)     = Cross (fmap specOfConf xs)


main :: IO ()
main = do
    putStrLn "----- AST -----"
    putStrLn ""
    print testAst
    putStrLn ""
    putStrLn "----- Typechecks -----"
    putStr $ (show . check) testAst
    return ()

