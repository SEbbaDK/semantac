{-# LANGUAGE NamedFieldPuns #-}

module Matcher where

import Ast
import Loc
import Types
import TypeChecker
import Pretty -- TEMP

import Control.Monad       (zipWithM)
import Control.Monad.State (State, evalState, get, evalStateT)
import qualified Data.Map as Map

type TypeMatcher = VariableExpr -> Type -> Bool
type MatcherState a = State (Specification, TypeMatcher) a

getSpec :: MatcherState Specification
getSpec = fmap fst get
getTypeMatcher :: MatcherState TypeMatcher
getTypeMatcher = fmap snd get

equalLength a b = length a == length b

-- Is sub a sub-type/compatible type of base
compatibleType :: Specification -> Type -> Type -> Bool
compatibleType spec base sub =
    case evalStateT unifyer initState of
        Right _ -> True
        Left _ -> False
        where
            unifyer = unify fakePos fakePos base sub
            initState = constructCheckerState spec []

confMatch :: Specification -> (TypeMap, Loc Conf) -> Conf -> Bool
confMatch spec (typemap, Loc _ (Conf a)) (Conf b) =
    evalState (confListMatch a b) (spec, varMatch)
        where
            varMatch :: TypeMatcher
            varMatch x y = case Map.lookup (rootVariable x) typemap of
                Just tx -> compatibleType spec tx y
                _       -> error "Don't give the matcher a typemap without all variables"

confListMatch :: [SyntaxList] -> [SyntaxList] -> MatcherState Bool
confListMatch a b = do
    matches <- fmap and $ zipWithM syntaxListMatch a b
    return $ equalLength a b && matches

syntaxListMatch :: SyntaxList -> SyntaxList -> MatcherState Bool
syntaxListMatch a b = do
    matches <- fmap and $ zipWithM syntaxMatch a b
    return $ equalLength a b && matches

syntaxMatch :: Loc SyntaxElem -> Loc SyntaxElem -> MatcherState Bool
syntaxMatch (Loc p1 t1) (Loc p2 t2) = case (t1,t2) of
    (Syntax s1 , Syntax s2 ) -> return $ s1 == s2
    (SubElem s1, SubElem s2) -> syntaxListMatch s1 s2
    (Var v1    , Var v2    ) -> variableCompare v1 (Loc p2 v2)
    (Var v1    , SubElem _ ) -> do tm <- getTypeMatcher; return $ tm v1 tSyntax
    _                        -> return $ False

variableCompare :: VariableExpr -> Loc VariableExpr -> MatcherState Bool
variableCompare v1 v2 = do
    (spec, typeMatcher) <- get
    let v1TypeName = typeName $ rootVariable $ unLoc v2
    case v1TypeName of
        Nothing -> error $ "Give type of " ++ pprint v2 ++ " variable"
        Just name -> case lookupTypeDirect name (sCategories spec) of
            Nothing -> error $ "No type called " ++ name
            Just (Loc _ cat) -> return $ typeMatcher v1 (cType cat)

