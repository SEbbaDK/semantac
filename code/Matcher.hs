module Matcher where

import Ast
import Loc
import Types
import TypeChecker

import Control.Monad       (zipWithM)
import Control.Monad.State (State, evalState, get)
import qualified Data.Map as Map

type StateTypeMaps a = State (TypeMap, TypeMap) a

equalLength a b = length a == length b

confMatch :: TypeMap -> TypeMap -> Loc Conf -> Loc Conf -> Bool
confMatch ltm rtm (Loc _ (Conf a)) (Loc _ (Conf b)) =
    evalState (confListMatch a b) (ltm, rtm)

confListMatch :: [SyntaxList] -> [SyntaxList] -> StateTypeMaps Bool
confListMatch a b = do
    matches <- fmap and $ zipWithM syntaxListMatch a b
    return $ equalLength a b && matches

syntaxListMatch :: SyntaxList -> SyntaxList -> StateTypeMaps Bool
syntaxListMatch a b = do
    matches <- fmap and $ zipWithM syntaxMatch a b
    return $ equalLength a b && matches

syntaxMatch :: Loc SyntaxElem -> Loc SyntaxElem -> StateTypeMaps Bool
syntaxMatch (Loc _ a) (Loc _ b) = case (a,b) of
    (Syntax s1 , Syntax s2 ) -> return $ s1 == s2
    (SubElem s1, SubElem s2) -> syntaxListMatch s1 s2
    (Var v1    , Var v2    ) -> variableCompare v1 v2
    _                        -> return $ False

variableCompare :: VariableExpr -> VariableExpr -> StateTypeMaps Bool
variableCompare v1 v2 = do
    (ltm, rtm) <- get
    let t1 = Map.lookup (rootVariable v1) ltm
    let t2 = Map.lookup (rootVariable v2) rtm
    case (t1, t2) of
        (Just type1, Just type2) -> return $ equivalentTypes type1 type2
        _                        -> error "Lookup into map failed. hmmm"

equivalentTypes :: Type -> Type -> Bool
equivalentTypes t1 t2 = error "unimplemented"

