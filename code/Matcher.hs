module Matcher where

import Ast
import Loc

import Control.Monad.State (State)

equalLength a b = length a == length b

confMatch :: (Loc Conf -> Loc Conf -> Bool
confMatch (Loc _ (Conf a)) (Loc _ (Conf b)) =
    confListMatch a b

confListMatch :: [SyntaxList] -> [SyntaxList] -> Bool
confListMatch (a:axs) (b:bxs) =
    equalLength a b && syntaxListMatch a b && confListMatch axs bxs
confListMatch [] [] =
    True
confListMatch _ _ =
    False -- This ensures an equal amount of parameters

syntaxListMatch :: SyntaxList -> SyntaxList -> Bool
syntaxListMatch a b =
    (equalLength a b) && (and $ zipWith syntaxMatch a b)

syntaxMatch :: Loc SyntaxElem -> Loc SyntaxElem -> Bool
syntaxMatch (Loc _ a) (Loc _ b) = case (a,b) of
    (Syntax s1 , Syntax s2 ) -> s1 == s2
    (Var v1    , Var v2    ) -> v1 == v2
    (SubElem s1, SubElem s2) -> syntaxListMatch s1 s2
    _ -> False

