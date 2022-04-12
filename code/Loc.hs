module Loc where

import           Data.List  (intercalate)
import           Data.Text  (splitOn)
import           Data.Tuple (fst)

type PosCoord = (String, Int, Int)
type Pos = (PosCoord, PosCoord)
data Loc a
  = Loc Pos a

unLoc :: Loc a -> a
unLoc (Loc _ a) = a

fakeLoc :: a -> Loc a
fakeLoc = Loc (("fake", 0, 0), ("fake", 0, 0))

instance Functor Loc where
  fmap f (Loc l v) = Loc l (f v)

instance (Show a) => Show (Loc a) where
  show (Loc _ a) = show a

coordFileName (n, _, _) = n
underline from to str =
  replicate from ' ' ++ replicate (to - from) '^'

showPosCoord (_, l, c) =
  "<" ++ show l ++ "," ++ show c ++ ">"

showPos (l, r) =
  showPosCoord l ++ " .. " ++ showPosCoord r

showLocInSource :: Loc a -> String -> String
showLocInSource (Loc pos _) src = intercalate "\n" $ fst $ foldr mark ([], 0) $ lines src
  where
    ((_,l1,c1), (_,l2,c2)) = pos
    mark line (res, index)
      | index == l1 && index == l2 = ([ line, underline c1 c2  line ] ++ res,    index + 1)
      | index == l1                = ([ line, underline c1 len line ] ++ res,    index + 1)
      | index == l2                = ([ line, underline 0  c2  line ] ++ res,    index + 1)
      | index < l1 && index > l2   = ([ line, underline 0  len line ] ++ res,    index + 1)
      | otherwise                  = (res, index + 1)
        where len = length line
