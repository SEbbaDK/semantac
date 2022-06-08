module OverlapChecker where

import Ast
import Errors
import Loc

import qualified Data.Map as Map

type OverlapResult = Maybe [DefinitionError]

overlapCheck :: Specification -> Maybe [SpecificationError]
overlapCheck (Specification terms cats sys rules) =
    fmap (map DefinitionError) $ mconcat
        [ overlapCheckTerms terms
        , overlapCheckCategories cats
        , overlapCheckSystems sys
        , overlapCheckRules rules
        ]

overlapCheckGen :: (a -> String)
                -> ([Loc a] -> DefinitionError)
                -> [Loc a]
                -> OverlapResult
overlapCheckGen getName err vals = justify $ map2err $ filterMulti $ mapConcat $ val2map vals
    where
        val2map = map $ \v -> Map.fromList [(getName $ unLoc v, [v])]
        mapConcat maps = Map.toList $ Map.unionsWith (++) maps
        filterMulti = filter $ \(k,v) -> length v > 1
        map2err = map (err . snd)
        justify l = if null l then Nothing else Just l

overlapCheckTerms = overlapCheckGen dName OverlappingTerms
overlapCheckCategories = overlapCheckGen cName OverlappingCategories
overlapCheckSystems = overlapCheckGen arrow OverlappingSystems
overlapCheckRules = overlapCheckGen name OverlappingRules

