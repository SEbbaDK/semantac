{-# LANGUAGE StandaloneDeriving #-}

module Binding where
    
import Ast
import Loc
import Errors
import Graph

import qualified Data.Set as Set
import Data.Set (Set, isSubsetOf, union, unions, intersection, difference)
import qualified Data.Map as Map
import Data.Map (Map)
import Debug.Trace -- TODO: Remove
import Data.List (nub)
unique = nub

type BindError = String -- SpecificationError
type BindResult = Maybe [BindError]
type BindResultOr a = Either [BindError] a

bindCheck :: Specification -> BindResult
bindCheck (Specification decs cats systems rules) = result
    where
        ruleChecks = map bindCheckRule rules
        result = foldr combineResults Nothing ruleChecks


bindCheckRule :: Loc Rule -> BindResult
bindCheckRule r =
  let
    (start, prems, end) = graphify $ unLoc r
    nodes = start : end : prems
    nodesAndVars = zip nodes $ map givenBy nodes
    res = reqSearch nodesAndVars [] nodes
  in
    case res of
        Left errors -> Just $ errors
        Right reqset -> Nothing -- TODO

reqSearch :: [(Node, Set Variable)] -> [Node] -> [Node] -> BindResultOr [Node]
reqSearch nv seen [] = Right $ unique seen
reqSearch nv seen (n:xs) = let
    reqs = reqsOf n
    pros = providers nv reqs reqs []
  in
    case pros of
        Left vars -> Left $
            [ show n ++ " has unfulfilled vars: " ++ unwords (map show $ Set.toList vars) ]
        Right nodes -> reqSearch nv (nodes ++ seen) xs

providers :: [(Node, Set Variable)] -> Set Variable -> Set Variable -> [Node] -> Either (Set Variable) [Node]
providers [] unseenVars _ nodes =
    if Set.null unseenVars
        then Right nodes
        else Left unseenVars
providers ((node, vars) : xs) unseenVars allVars nodes =
    if Set.null (vars `intersection` allVars)
        then providers xs unseenVars allVars nodes
        else providers xs (unseenVars `difference` vars) allVars (node : nodes)

reqsOf :: Node -> Set Variable
reqsOf (InitNode _) = Set.empty
reqsOf (ConcNode c) = varsOfConf c
reqsOf (TransNode (Transition _ before _)) = varsOfConf before
reqsOf (DefNode _ e) = varsOfExpr e
reqsOf (ConstrNode e) = varsOfExpr e

givenBy :: Node -> Set Variable
givenBy (InitNode c) = varsOfConf c
givenBy (ConcNode _) = Set.empty
givenBy (TransNode (Transition _ _ after)) = varsOfConf after
givenBy (DefNode e _) = varsOfExpr e
givenBy (ConstrNode _) = Set.empty

combineResults :: BindResult -> BindResult -> BindResult
combineResults Nothing o = o
combineResults o Nothing = o
combineResults (Just e1) (Just e2) = Just $ e1 ++ e2

varsOfVarExpr :: VariableExpr -> Set Variable
varsOfVarExpr (VBind v l r) = Set.insert l $ varsOfVarExpr v `union` varsOfExpr r
varsOfVarExpr (VRef v) = Set.singleton v

varsOfExpr :: Expr -> Set Variable
varsOfExpr (EVar  v)   = varsOfVarExpr v
varsOfExpr (ECall e p) = (varsOfExpr e) `union` (unions $ map varsOfExpr p)
varsOfExpr (EEq   l r) = varsOfExpr l `union` varsOfExpr r
varsOfExpr (EInEq l r) = varsOfExpr l `union` varsOfExpr r

varsOfConf :: Loc Conf -> Set Variable
varsOfConf (Loc _ c) = case c of
    Conf cs       -> unions $ map varsOfConf cs
    Syntax _      -> Set.empty
    Var v         -> varsOfVarExpr v
    SyntaxList cs -> unions $ map varsOfConf cs
    Paren c       -> varsOfConf c

