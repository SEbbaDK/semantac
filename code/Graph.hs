{-# LANGUAGE NamedFieldPuns #-}

module Graph where

import Ast
import Loc

type Graph = (Node, [Node], Node)
data Node = InitNode (Loc Conf)
          | ConcNode (Loc Conf)
          | PremNode (Loc Premise)
    deriving (Show, Eq)

graphify :: Rule -> Graph
graphify Rule { name, base, premises, properties } =
    (start, prems, end)
    where
        Loc _ Transition { system, before, after } = base
        start = InitNode before
        end   = ConcNode after
        prems = map PremNode premises

nodePos (InitNode (Loc p _)) = p
nodePos (ConcNode (Loc p _)) = p
nodePos (PremNode (Loc p _)) = p
 
