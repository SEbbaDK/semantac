{-# LANGUAGE NamedFieldPuns #-}

module Pretty where

import           Data.List (intercalate, intersperse)

import Ast
import Loc
import Types

class Pretty a where
      pprint :: a -> String

instance Pretty a => Pretty (Loc a) where
  pprint (Loc _ a) = pprint a

instance Pretty Specification where
  pprint (Specification declarations categories systems rules) =
    unlines lines
    where
      lines = intercalate [""]
        [ map pprint declarations
        , map pprint categories
        , map pprint systems
        , intersperse "" (map pprint rules)
        ]

instance Pretty Declaration where
  pprint (Declaration name spec) =
    unwords [ "meta", name, "=", pprint spec ]

instance Pretty Category where
  pprint Category {cName, variable, cType} =
    unwords [ "category", variable, "in", cName, ":", pprint cType ]

instance Pretty System where
  pprint System {arrow, initial, final} =
    unwords [ "system", pprint initial, arrow, pprint final ]

instance Pretty Rule where
  pprint Rule {name, base, premises, properties} =
    unlines lines
    where
      center len str = let diff = len - length str
                       in  replicate (diff `div` 2) ' ' ++ str
      premisesStrs = map pprint premises
      baseStr = pprint base
      sepLength = maximum (map length (baseStr : premisesStrs))
      centeredPremStrs = map (center sepLength) premisesStrs
      centeredBaseStr = center sepLength baseStr
      lines = concat
        [ map pprint properties
        , [ name ++ ":" ]
        , centeredPremStrs
        , [ replicate sepLength '-' | not (null premises) ]
        , [ centeredBaseStr ]
        ]


instance Pretty Property where
  pprint NonDeterministic = "non-deterministic"
  pprint NonTerminating   = "non-terminating"

instance Pretty Transition where
  pprint Transition { system, before, after } =
    pprint before ++ " " ++ system ++ " " ++ pprint after

instance Pretty Variable where
  pprint Variable { typename, varname, marks, binds } = concat
    [ typename
    , varnamer varname
    , replicate marks '\''
    , concatMap (\(var, val) -> "[" ++ pprint var ++ "↦" ++ pprint val ++ "]") binds
    ]
    where
        varnamer ""  = ""
        varnamer "0" = "₀"
        varnamer "1" = "₁"
        varnamer "2" = "₂"
        varnamer "3" = "₃"
        varnamer "4" = "₄"
        varnamer "5" = "₅"
        varnamer "6" = "₆"
        varnamer "7" = "₇"
        varnamer "8" = "₈"
        varnamer "9" = "₉"
        varnamer nam = "_" ++ nam

instance Pretty Conf where
  pprint (Conf s)        = "⟨" ++ intercalate ", " (map pprint s) ++ "⟩"
  pprint (Paren e)       = "(" ++ pprint e ++ ")"
  pprint (SyntaxList xs) = unwords $ fmap pprint xs
  pprint (Syntax s)      = "\"" ++ s ++ "\""
  pprint (Var v)         = pprint v

instance Pretty Premise where
  pprint (PTransition trans) = pprint trans
  pprint (PEquality eq)      = pprint eq

instance Pretty Equality where
  pprint (Eq l r)   = pprint l ++ " = " ++ pprint r
  pprint (InEq l r) = pprint l ++ " ≠ " ++ pprint r

instance Pretty Expr where
  pprint (EVar s)    = pprint s
  pprint (ECall o e) = pprint o ++ "(" ++ intercalate ", " (map pprint e) ++ ")"

instance Pretty Type where
  pprint (TNamed name)    = name
  pprint (TCategory name) = name
  pprint (TCross xs)      = intercalate " ⨯ " (fmap pprint xs)
  pprint (TUnion xs)      = intercalate " ∪ " (fmap pprint xs)
  pprint (TFunc a b)      = pprint a ++ " → " ++ pprint b
  pprint (TVar tv)        = pprint tv

instance Pretty TypeVar where
  pprint (TypeVar v) = "#" ++ reverse (intToAlphaRev v)
      where
          intToAlphaRev n | n <= 0 = []
          intToAlphaRev n =
              let (q, r) = quotRem (n - 1) 26 in
              toEnum (fromEnum 'a' + r) : intToAlphaRev q

