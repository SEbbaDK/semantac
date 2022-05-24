{-# LANGUAGE NamedFieldPuns #-}

module Graph where

import Ast
import Loc

type Graph = (Node, [Node], Node)
data Node = InitNode (Loc Conf)
          | ConcNode (Loc Conf)
          | TransNode Transition
          | DefNode Expr Expr
          | ConstrNode Expr
    deriving (Show, Eq)

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
premiseToNode (PConstraint e)   = ConstrNode e

