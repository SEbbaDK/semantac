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

pos :: Loc a -> Pos
pos (Loc p _) = p

fakeLoc :: a -> Loc a
fakeLoc = Loc (("fake", 0, 0), ("fake", 0, 0))

instance Functor Loc where
  fmap f (Loc l v) = Loc l (f v)

instance (Show a) => Show (Loc a) where
  show (Loc _ a) = show a

coordFileName (n, _, _) = n

showPosCoord (f, l, c) =
  f ++ ":"  ++ show l ++ ":" ++ show c

showPos ((f,l1,c1), (_,l2,c2)) =
  f ++ ":" ++ show l1 ++ ":" ++ show c1 ++ " to " ++ show l2 ++ ":" ++ show c2

underline from to str =
  let from1 = from - 1
  in replicate from1 ' ' ++ replicate (to - from1) '^'

leftPadded count str =
  let dif = count -  length str
  in replicate (max 0 dif) ' ' ++ str

dividedNumberedLines divider number lines =
  let num   = show number
      numl  = length num
      first = leftPadded 4 num ++ divider ++ head lines
      rest  = map (\l -> replicate 4 ' ' ++ divider ++ l) (tail lines)
  in first : rest

showPosInSource :: Pos -> String -> String
showPosInSource pos src = unlines $ fst $ foldl mark ([], 1) $ lines src
  where
    ((_,l1,c1), (_,l2,c2)) = pos
    mark (res, index) line
      | index == l1 && index == l2 = (res ++ lined (l1,l2) index c1 c2  line, index + 1)
      | index == l1                = (res ++ lined (l1,l2) index c1 len line, index + 1)
      | index == l2                = (res ++ lined (l1,l2) index 0  c2  line, index + 1)
      | index > l1 && index < l2   = (res ++ lined (l1,l2) index 0  len line, index + 1)
      | otherwise                  = (res, index + 1)
        where len = length line
              lined (l1,l2) index c1 c2 line | l1 == l2 =
                dividedNumberedLines " | " index [ line, underline c1 c2 line ]
              lined (l1,l2) index c1 c2 line =
                dividedNumberedLines " | " index [ line ]


showLocInSource :: Loc a -> String -> String
showLocInSource (Loc pos _) src = showPosInSource pos src

