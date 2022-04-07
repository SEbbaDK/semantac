{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ast where

import           Data.List (intercalate, intersperse)
import           Loc

data Top
  = Top [Loc Declaration] [Loc Category] [Loc System] [Loc Rule]

data Declaration =
  Declaration String Spec

data Category
  = Category
    { category :: String
    , variable :: String
    , spec     :: Spec
    }

data System
  = System
    { arrow   :: String
    , initial :: Spec
    , final   :: Spec
    }

instance Show Top where
  show (Top declarations categories systems rules) =
    intercalate "\n" lines
    where
      lines = concat
        [ map show declarations
        , [""]
        , map show categories
        , [""]
        , map show systems
        , [""]
        , intersperse "" (map show rules)
        ]

instance Show Declaration where
  show (Declaration name spec) =
    unwords [ "meta", name, "=", show spec ]

instance Show Category where
  show Category {category, variable, spec} =
    unwords [ "category", variable, "in", category, ":", show spec ]

instance Show System where
  show System {arrow, initial, final} =
    unwords [ "system", show initial, arrow, show final ]

data Spec
  = Integer
  | Identifier
  | SSyntax
  | Custom String
  | Cross [Spec]
  | Union [Spec]
  | Func Spec Spec
deriving instance Eq Spec

instance Show Spec where
  show Integer       = "Integer"
  show Identifier    = "Identifier"
  show SSyntax       = "Syntax"
  show (Custom name) = name
  show (Cross xs)    = intercalate " ⨯ " (fmap show xs)
  show (Union xs)    = intercalate " ∪ " (fmap show xs)
  show (Func a b)    = show a ++ " → " ++ show b

data Rule
  = Rule
    { name       :: String
    , base       :: Trans
    , premises   :: [Premise]
    , properties :: [Property]
    }

instance Show Rule where
  show Rule {name, base, premises, properties} =
    intercalate "\n" lines
    where
      center len str = let diff = len - length str
                       in  replicate (diff `div` 2) ' ' ++ str

      premisesStrs = map show premises
      baseStr = show base
      sepLength = maximum (map length (baseStr : premisesStrs))
      centeredPremStrs = map (center sepLength) premisesStrs
      centeredBaseStr = center sepLength baseStr
      lines = concat
        [ map show properties
        , [ name ++ ":" ]
        , centeredPremStrs
        , [ replicate sepLength '-' | not (null premises) ]
        , [ centeredBaseStr ]
        ]

data Property = NonDeterministic | NonTerminating

instance Show Property where
  show NonDeterministic = "non-deterministic"
  show NonTerminating   = "non-terminating"

data Trans
  = Trans
    { system :: String
    , before :: Loc Conf
    , after  :: Loc Conf
    }

instance Show Trans where
  show Trans { system, before, after } =
    show before ++ " " ++ system ++ " " ++ show after

data Variable
  = Variable
    { typename :: String
    , varname  :: String
    , marks    :: Int
    , binds    :: [(Variable, Variable)]
    }

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

instance Show Variable where
  show Variable { typename, varname, marks, binds } = concat
    [ typename
    , varnamer varname
    , replicate marks '\''
    , concatMap (\(var, val) -> "[" ++ show var ++ "↦" ++ show val ++ "]") binds
    ]

data Conf
  = Conf [Loc Conf]
  | Syntax String
  | Var Variable
  | SyntaxList [Loc Conf]
  | Paren (Loc Conf)

instance Show Conf where
  show (Conf s)        = "⟨" ++ intercalate ", " (map show s) ++ "⟩"
  show (Paren e)       = "(" ++ show e ++ ")"
  show (SyntaxList xs) = unwords $ fmap show xs
  show (Syntax s)      = "\"" ++ s ++ "\""
  show (Var v)         = show v

data Premise
  = TPremise Trans
  | TEquality Equality

instance Show Premise where
  show (TPremise trans) = show trans
  show (TEquality eq)   = show eq

data Equality
  = Eq Expr Expr
  | InEq Expr Expr

instance Show Equality where
  show (Eq l r)   = show l ++ " = " ++ show r
  show (InEq l r) = show l ++ " ≠ " ++ show r

data Expr
  = EVar Variable
  | ECall Expr [Expr]

instance Show Expr where
  show (EVar s)  = show s
  show (ECall o e) = show o ++ "(" ++ intercalate ", " (map show e) ++ ")"

