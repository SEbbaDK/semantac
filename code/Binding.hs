{-# LANGUAGE NamedFieldPuns #-}

module Binding where
    
import Ast
import Loc
import Errors

import Data.Set (Set)
import Debug.Trace -- TODO: Remove

type BindResult = Maybe [SpecificationError]

type Graph = (Node, [Node], Node)
data Node = InitNode (Loc Conf)
          | ConcNode
          | TransNode Transition
          | DefNode Expr Expr
          | LeftDefNode Expr Expr
          | EqNode Equality

graphify :: Rule -> Graph
graphify Rule { name, base, premises, properties } =
    (start, nodes, end)
    where
        Transition { system, before, after } = base
        start = EndNode Base before
        end   = EndNode Conc after
        nodes = map nodeify premises

nodeify :: Premise -> Node
nodeify (PTransition t)   = TransNode t
nodeify (PDefinition a b) = DefNode a b
nodeify (PEquality e)     = EqNode e

bindCheck :: Specification -> BindResult
bindCheck (Specification decs cats systems rules) = result
    where
        ruleChecks = map bindCheckRule rules
        result = foldl combineResults Nothing ruleChecks


bindCheckRule :: Loc Rule -> BindResult
bindCheckRule r = Nothing
    where
        graph = graphify $ unLoc r
        reqs = traceShowId $ reqSearch graph

reqsOf :: Node -> Set Variable
reqsOf (InitNode _) = Set.empty
reqsOf (ConcNode c) = varsOfConf c
reqsOf (TransNode (Transition _ before _)) = varsOfConf before
reqsOf (DefNode _ _) = error "This should have been collapsed"
reqsOf (LeftDefNode _ e) = varsOfExpr e
reqsOf (EqNode l r) = varsOfExpr l `Set.union` varsOfExpr r

givenBy :: Node -> Set Variable
givenBy (InitNode _) = Set.empty
givenBy (ConcNode c) = varsOfConf c
givenBy (TransNode (Transition _ _ after)) = varsOfConf after
givenBy (DefNode _ _) = error "This should have been collapsed"
givenBy (LeftDefNode e _) = varsOfExpr e
givenBy (EqNode _ _) = Set.empty

reqSearch :: Graph -> (BindResult, Graph)
reqSearch (start, nodes, end) = reqSearch_ [] [end] [] []
    where
        reqSearch_ visitLater (next:rest) visited path =

        -- When we are done on this level, continue
        reqSearch_ visitLater [] visited path =
            reqSearch_ [] visitLater [] (visited : path)

combineResults :: BindResult -> BindResult -> BindResult
combineResults Nothing o = o
combineResults o Nothing = o
combineResults (Just e1) (Just e2) = Just $ e1 ++ e2

