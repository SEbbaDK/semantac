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

type BindError = SpecificationError
type BindResult = Maybe [BindError]
type BindResultOr a = Either [BindError] a

type BindRuleError = RuleError
type BindRuleResult = Maybe [BindRuleError]
type BindRuleResultOr a = Either [BindRuleError] a

bindCheck :: Specification -> BindResult
bindCheck (Specification decs cats systems rules) = result
    where
        ruleChecks = map bindCheckRule rules
        result = concatMaybe ruleChecks


bindCheckRule :: Loc Rule -> BindResult
bindCheckRule r =
  let
    (start, prems, end) = graphify $ unLoc r
    nodes = start : end : prems
    backRes = backwardSearch nodes
    foreRes = forwardSearch nodes
    errorify e = Just $ map (RuleError $ name $ unLoc r) e
  in
    case (backRes, foreRes) of
      (Left be, Left fe) -> errorify $ be ++ fe
      (Left be, _)       -> errorify be
      (_      , Left fe) -> errorify fe
      (Right b, Right f) -> if null untouched
        then Nothing
        else errorify $ map UnreachablePremise untouched
          where untouched = filter (\x -> notElem x b) $ filter (\x -> notElem x f) $ nodes

backwardSearch nodes = search UndefinedVar reqsOf (zip nodes $ map givenBy nodes) [] nodes
forwardSearch nodes = search UnusedVar givenBy (zip nodes $ map reqsOf nodes) [] nodes

-- Searches either towards base or conclusion
search :: (Node -> [Variable] -> RuleError) -> (Node -> Set Variable) -> [(Node, Set Variable)] -> [Node] -> [Node] -> BindRuleResultOr [Node]
search err varFunc nv seen [] = Right $ unique seen
search err varFunc nv seen (n:xs) = let
    reqs = varFunc n
    pros = sources nv reqs reqs []
  in
    case pros of
        Left vars -> Left $
            [ err n $ Set.toList vars ]
        Right nodes -> search err varFunc nv (nodes ++ seen) xs

-- Finds all variables that supplies parts of the set
-- Throws right if enough sources are found
-- Throws left if not all sources are found
sources :: [(Node, Set Variable)] -> Set Variable -> Set Variable -> [Node] -> Either (Set Variable) [Node]
sources [] unseenVars _ nodes =
    if Set.null unseenVars
        then Right nodes
        else Left unseenVars
sources ((node, vars) : xs) unseenVars allVars nodes =
    if Set.null (vars `intersection` allVars)
        then sources xs unseenVars allVars nodes
        else sources xs (unseenVars `difference` vars) allVars (node : nodes)

reqsOf :: Node -> Set Variable
reqsOf (InitNode _) = Set.empty
reqsOf (ConcNode c) = varsOfConf c
reqsOf (PremNode (Loc _ prem)) = case prem of
  PTransition (Transition _ before _) -> varsOfConf before
  PDefinition _ e                     -> varsOfExpr e
  PConstraint e                       -> varsOfExpr e

givenBy :: Node -> Set Variable
givenBy (InitNode c) = varsOfConf c
givenBy (ConcNode _) = Set.empty
givenBy (PremNode (Loc _ prem)) = case prem of
  PTransition (Transition _ _ after)  -> varsOfConf after
  PDefinition e _                     -> varsOfExpr e
  PConstraint _                       -> Set.empty

concatMaybe :: [Maybe [a]] -> Maybe [a]
concatMaybe = foldr combineMaybe Nothing

combineMaybe :: Maybe [a] -> Maybe [a] -> Maybe [a]
combineMaybe Nothing o = o
combineMaybe o Nothing = o
combineMaybe (Just e1) (Just e2) = Just $ e1 ++ e2

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

