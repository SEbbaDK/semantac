{-# LANGUAGE StandaloneDeriving #-}

module Binding where
    
import Ast
import Errors
import Graph
import Loc
import Types

import qualified Data.Set as Set
import Data.Set (Set, isSubsetOf, union, unions, intersection, difference)
import qualified Data.Map as Map
import Debug.Trace -- TODO: Remove
import Data.List (nub)
unique = nub

type BindError = SpecificationError
type BindResult = Maybe [BindError]
type BindResultOr a = Either [BindError] a

type BindRuleError = RuleError
type BindRuleResult = Maybe [BindRuleError]
type BindRuleResultOr a = Either [BindRuleError] a

type TermLookup = (String -> Maybe Type)

bindCheck :: Specification -> Maybe [Error BindError]
bindCheck (Specification decs cats systems rules) = result
    where
        lookupper :: TermLookup
        lookupper name = fmap (dType . unLoc) $ termLookup name decs
        ruleChecks = concatMaybe $ map (bindCheckRule lookupper) rules
        mapper :: SpecificationError -> Error SpecificationError
        mapper e = case e of
            RuleError r e -> Error ([CRule r], RuleError r e)
            x -> Error ([], x)
        result = fmap (map mapper) ruleChecks

bindCheckRule :: TermLookup -> Loc Rule -> BindResult
bindCheckRule t r =
  let
    (start, prems, end) = graphify $ unLoc r
    nodes = start : end : prems
    backRes = backwardSearch nodes end
    foreRes = forwardSearch nodes start
    errorify e = Just $ map (RuleError r) e
  in
    case (backRes, foreRes) of
      (Left be, Left fe) -> errorify $ be ++ fe
      (Left be, _)       -> errorify be
      (_      , Left fe) -> errorify fe
      (Right b, Right f) -> if null untouched
        then Nothing
        else errorify $ map UnreachablePremise untouched
          where untouched = filter (\x -> notElem x b && notElem x f) nodes

forwardSearch :: [Node] -> Node -> BindRuleResultOr [Node]
forwardSearch nodes start =
    search UnusedVar s [] [start]
        where
            s n = sources (zip nodes $ map reqsOf nodes) (givenBy n) (givenBy n) []
backwardSearch :: [Node] -> Node -> BindRuleResultOr [Node]
backwardSearch nodes end =
    checkDupes nodes $ search UndefinedVar s [] [end]
        where
            s n = sources (zip nodes $ map givenBy nodes) (reqsOf n) (reqsOf n) []
            checkDupes nodes srch = let
                node2varnodemap n = Map.fromList $ map (\v -> (v,[n])) $ Set.toList $ givenBy n
                var2nodes = foldl (Map.unionWith (++)) Map.empty $ map node2varnodemap nodes
                multidefines = filter (\(v,ns) -> length ns > 1) $ Map.toList var2nodes
              in
                case null multidefines of
                  True  -> srch
                  False -> Left $ map (\(v,n) -> MultidefinedVar v n) multidefines

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

concatMaybe :: [Maybe [a]] -> Maybe [a]
concatMaybe = foldr combineMaybe Nothing

combineMaybe :: Maybe [a] -> Maybe [a] -> Maybe [a]
combineMaybe Nothing o = o
combineMaybe o Nothing = o
combineMaybe (Just e1) (Just e2) = Just $ e1 ++ e2

-- TODO: This is probably incorrect. The bindings should not count as definitions
--       but as uses when it comes to requirements etc.
varsOfVarExpr :: VariableExpr -> Set Variable
varsOfVarExpr (VBind v l r) = Set.insert l $ varsOfVarExpr v `union` varsOfExpr r
varsOfVarExpr (VRef v) = Set.singleton v

varsOfExpr :: Expr -> Set Variable
varsOfExpr (EVar  v)   = varsOfVarExpr v
varsOfExpr (ELit _ _)  = Set.empty
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

