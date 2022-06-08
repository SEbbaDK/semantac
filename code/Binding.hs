{-# LANGUAGE StandaloneDeriving #-}

module Binding where
    
import Ast
import Errors
import Graph
import Loc
import Types

import Data.Bifunctor (first)
import qualified Data.Set as Set
import Data.Set (Set, isSubsetOf, union, unions, intersection, difference)
import qualified Data.Map as Map
import Data.List (nub)
unique = nub

type BindError = SpecificationError
type BindResult = Maybe [BindError]
type BindResultOr a = Either [BindError] a

type BindRuleError = RuleError
type BindRuleResult = Maybe [BindRuleError]
type BindRuleResultOr a = Either [BindRuleError] a

type TermLookup = (String -> Maybe Type)
type VarFilter = Set Variable -> Set Variable

bindCheck :: Specification -> Maybe [Error BindError]
bindCheck (Specification decs cats systems rules) =
    let
        lookupper :: TermLookup
        lookupper name = fmap (dType . unLoc) $ termLookup name decs
        ruleChecks = mconcat $ map (bindCheckRule lookupper) rules
        mapper :: SpecificationError -> Error SpecificationError
        mapper e = case e of
            RuleError r e -> Error ([CRule r], RuleError r e)
            x -> Error ([], x)
        result = fmap (map mapper) ruleChecks
    in
        case result of
            Just [] -> Nothing
            r       -> r

bindCheckRule :: TermLookup -> Loc Rule -> BindResult
bindCheckRule t r =
    let
        (start, prems, end) = graphify $ unLoc r
        nodes = start : end : prems

        varIsNotTerm v = case v of
            Variable Nothing n 0 False False -> (t n) == Nothing
            otherwise -> True
        filterTerms = Set.filter varIsNotTerm

        backRes = backwardSearch filterTerms nodes end
        foreRes = forwardSearch filterTerms nodes start
        searchErrs = combineSearches nodes backRes foreRes
        dupeErrs = checkDupes nodes
        errorify = map (RuleError r)
    in
        fmap errorify $ dupeErrs `mappend` searchErrs

combineSearches nodes backRes foreRes =
    case (backRes, foreRes) of
      (Left be, Left fe) -> Just $ be ++ fe
      (Left be, _)       -> Just be
      (_      , Left fe) -> Just fe
      (Right b, Right f) -> if null untouched
        then Nothing
        else Just $ map UnreachablePremise untouched
          where untouched = filter (\x -> notElem x b && notElem x f) nodes

checkDupes :: [Node] -> BindRuleResult
checkDupes nodes = let
    node2varnodemap n = Map.fromList $ map (\v -> (v,[n])) $ Set.toList $ givenBy n
    var2nodes = foldl (Map.unionWith (++)) Map.empty $ map node2varnodemap nodes
    multidefines = filter (\(v,ns) -> length ns > 1) $ Map.toList var2nodes
  in
    case null multidefines of
      True  -> Nothing
      False -> Just $ map (\(v,n) -> MultidefinedVar v n) multidefines

forwardSearch :: VarFilter -> [Node] -> Node -> BindRuleResultOr [Node]
forwardSearch termfilter nodes start =
    first removeUnused $ search UnusedVar s [] [start]
        where
            s n = sources (zip nodes $ map (termfilter . reqsOf) nodes) (termfilter $ givenBy n) (termfilter $ givenBy n) []
            mapUnused errs (UnusedVar n vars) = if null filtered
                then errs
                else (UnusedVar n filtered) : errs
                    where filtered = filter (not . unused) vars
            mapUnused errs _ = errs
            removeUnused errs = foldl mapUnused [] errs

backwardSearch :: VarFilter -> [Node] -> Node -> BindRuleResultOr [Node]
backwardSearch termfilter nodes end =
     search UndefinedVar s [] [end]
        where
            s n = sources (zip nodes $ map (termfilter . givenBy) nodes) (termfilter $ reqsOf n) (termfilter $ reqsOf n) []

-- Searches either towards base or conclusion
search :: (Node -> [Variable] -> RuleError)
       -> (Node -> Either (Set Variable) [Node])
       -> [Node]
       -> [Node]
       -> BindRuleResultOr [Node]
search err s seen []     = Right $ unique seen
search err s seen (n:xs) =
    case s n of
        Left vars -> Left $
            [ err n $ Set.toList vars ]
        Right nodes -> search err s (n : seen) (xs ++ unseen)
            where unseen = filter (\n -> n `notElem` seen) nodes

-- Finds all variables that supplies parts of the set
-- Throws right if enough sources are found
-- Throws left if not all sources are found
sources :: [(Node, Set Variable)]
        -> Set Variable
        -> Set Variable
        -> [Node]
        -> Either (Set Variable) [Node]
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

-- TODO: This is probably incorrect. The bindings should not count as definitions
--       but as uses when it comes to requirements etc.
varsOfVarExpr :: VariableExpr -> Set Variable
varsOfVarExpr (VBind v l r) = Set.insert l $ varsOfVarExpr v `union` varsOfExpr r
varsOfVarExpr (VRef v) = case literal v of
    True  -> Set.empty
    False -> Set.singleton v

varsOfExpr :: Expr -> Set Variable
varsOfExpr (EVar  v)   = varsOfVarExpr v
varsOfExpr (ECall e p) = (varsOfExpr e) `union` (unions $ map varsOfExpr p)
varsOfExpr (EEq   l r) = varsOfExpr l `union` varsOfExpr r
varsOfExpr (EInEq l r) = varsOfExpr l `union` varsOfExpr r

varsOfConf :: Loc Conf -> Set Variable
varsOfConf (Loc _ (Conf cs)) = unions $ map unions $ map (map varsOfSyntaxElems) cs

varsOfSyntaxElems :: Loc SyntaxElem -> Set Variable
varsOfSyntaxElems (Loc _ e) = case e of
    Syntax _  -> Set.empty
    Var v     -> varsOfVarExpr v
    SubElem c -> unions $ map varsOfSyntaxElems c

