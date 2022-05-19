{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Binding where
    
import Ast
import Loc
import Errors

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

type Graph = (Node, [Node], Node)
data Node = InitNode (Loc Conf)
          | ConcNode (Loc Conf)
          | TransNode Transition
          | DefNode Expr Expr
          | EqNode Equality
    deriving (Show, Eq)

deriving instance (Eq Equality)
deriving instance (Eq Transition)
deriving instance (Eq Expr)
deriving instance (Eq Conf)

deriving instance (Ord Equality)
deriving instance (Ord Transition)
deriving instance (Ord Expr)
deriving instance (Ord Conf)

instance Eq a => Eq (Loc a) where
    (==) (Loc _ x) (Loc _ y) =
        x == y
instance Ord a => Ord (Loc a) where
    compare (Loc _ x) (Loc _ y) =
        compare x y
instance Eq Variable where
    (==) (Variable t1 n1 m1 b1) (Variable t2 n2 m2 b2) =
        t1 == t2 && n1 == n2 && m1 == m2
instance Ord Variable where
    compare (Variable t1 n1 m1 b1) (Variable t2 n2 m2 b2) = let
        tc = compare t1 t2
        nc = compare n1 n2
        mc = compare m1 m2
      in
        if tc /= EQ
            then tc
            else if nc /= EQ
                then nc
                else mc
    (<=) v1 v2 =
        GT /= compare v1 v2

graphify :: Rule -> Graph
graphify Rule { name, base, premises, properties } =
    (start, prems, end)
    where
        Transition { system, before, after } = base
        start = InitNode before
        end   = ConcNode after
        prems = map premiseToNode premises

premiseToNode :: Premise -> Node
premiseToNode (PTransition t)   = TransNode t
premiseToNode (PDefinition a b) = DefNode a b
premiseToNode (PEquality e)     = EqNode e

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
reqsOf (EqNode (Eq l r)) = varsOfExpr l `union` varsOfExpr r
reqsOf (EqNode (InEq l r)) = varsOfExpr l `union` varsOfExpr r

givenBy :: Node -> Set Variable
givenBy (InitNode c) = varsOfConf c
givenBy (ConcNode _) = Set.empty
givenBy (TransNode (Transition _ _ after)) = varsOfConf after
givenBy (DefNode e _) = varsOfExpr e
givenBy (EqNode _) = Set.empty

combineResults :: BindResult -> BindResult -> BindResult
combineResults Nothing o = o
combineResults o Nothing = o
combineResults (Just e1) (Just e2) = Just $ e1 ++ e2

varsOfExpr (EVar v) = Set.singleton v
varsOfExpr (ECall e p) = (varsOfExpr e) `union` (unions $ map varsOfExpr p)

varsOfConf :: Loc Conf -> Set Variable
varsOfConf (Loc _ c) = case c of
    Conf cs       -> unions $ map varsOfConf cs
    Syntax _      -> Set.empty
    Var v         -> Set.singleton v
    SyntaxList cs -> unions $ map varsOfConf cs
    Paren c       -> varsOfConf c

