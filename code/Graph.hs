module Graph where

import Binding

dotString s = "\"" ++ replace '"' "\\\"" s ++ "\""
    where replace char string l = concat $ map (\c -> if c == char then string else [c]) l

dotGraph :: Graph -> String
dotGraph (start, prems, end) = unlines $ concat $
    [ [ "digraph G {" ]
    , [ "node [shape=circle,margin=0,height=0]" ]
    , concat $ map dotNode (start : end : prems)
    , [ "}" ]
    ]

edge :: Show a => Show b => a -> b -> String
edge from to = (dotString $ show from) ++ " -> " ++ (dotString $ show to)

dotNode :: Node -> [String]
dotNode n = map (\v -> edge v n) (Set.toList $ reqsOf n) ++
            map (\v -> edge n v) (Set.toList $ givenBy n) ++
            [ case n of
                InitNode c -> dotString (show n) ++ " [shape=rectangle,style=dashed]"
                ConcNode c -> dotString (show n) ++ " [shape=rectangle,style=dashed]"
                otherwise -> dotString (show n) ++ " [shape=rectangle]"
            ]

