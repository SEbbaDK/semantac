{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ast where

import           Data.List (intercalate, intersperse)

data Top
  = Top [Category] [System] [Rule]

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
  show (Top categorys systems rules) =
    intercalate "\n" lines
    where
      lines = concat
        [ map show categorys
        , [""]
        , map show systems
        , [""]
        , intersperse "" (map show rules)
        ]

instance Show Category where
  show Category {category, variable, spec} =
    unwords ["category", variable, "in", category, ":", show spec]

instance Show System where
  show System {arrow, initial, final} =
    unwords ["system", show initial, arrow, show final]

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
  show (Cross xs)    = intercalate " * " (fmap show xs)
  show (Union xs)    = intercalate " | " (fmap show xs)
  show (Func a b)    = (show a) ++ " → " ++ (show b)

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
      premisesStrs = map show premises
      baseStr = show base
      sepLength = maximum (map length (baseStr : premisesStrs))
      lines = concat
        [ map show properties
        , [name ++ ":" ]
        , premisesStrs
        , [replicate sepLength '-' | not (null premises)]
        , [baseStr]
        ]

data Property = NonDeterministic | NonTerminating

instance Show Property where
  show NonDeterministic = "non-deterministic"
  show NonTerminating   = "non-terminating"

data Trans
  = Trans
    { system :: String
    , before :: Conf
    , after  :: Conf
    }

instance Show Trans where
  show Trans {system, before, after} =
    show before ++ " " ++ system ++ " " ++ show after

data Conf
  = Syntax String
  | Variable String String Int -- base subscript marks
  | Tup [Conf]
  | SupTup [Conf]

instance Show Conf where
  show (Syntax s)   = "\"" ++ s ++ "\""
  show (Variable x s m) = concat
    [ x
    , if s == "" then "" else "_" ++ s
    , replicate m '\''
    ]
  show (Tup xs)     = "<" ++ unwords (fmap show xs) ++ ">"
  show (SupTup xs)  = unwords (fmap show xs)

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
  = EVar String
  | EOp String [Expr]

instance Show Expr where
  show (EVar s)  = s
  show (EOp o e) = o ++ " " ++ show e

