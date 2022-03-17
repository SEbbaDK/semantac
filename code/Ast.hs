{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ast where

import           Data.List (intercalate, intersperse)

data Top
  = Top [Domain] [System] [Rule]

data Domain
  = Domain
    { domain :: String
    , spec   :: Spec
    }

data System
  = System
    { arrow   :: String
    , initial :: Spec
    , final   :: Spec
    }

instance Show Top where
  show (Top domains systems rules) =
    intercalate "\n" lines
    where
      lines = concat
        [ map show domains
        , map show systems
        , map show rules
        ]

instance Show Domain where
  show Domain {domain, spec} =
    domain ++ " : " ++ show spec

instance Show System where
  show System {arrow, initial, final} =
    arrow ++ " = " ++ show initial ++ " × " ++ show final

data Spec
  = Integer
  | Identifier
  | Custom String
  | Cross Spec Spec
  | Union Spec Spec

instance Show Spec where
  show Integer       = "Integer"
  show Identifier    = "Identifier"
  show (Custom name) = name
  -- TODO: these unicode characters display as a '?' in my terminal...
  show (Cross l r)   = show l ++ " × " ++ show r
  show (Union l r)   = show l ++ " ∪ " ++ show r

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
      lines = concat
        [ map show properties
        , [name ++ ":" ]
        , map show premises
        , ["------"]
        , [show base]
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

newtype Conf
  = Conf [Elem]

instance Show Conf where
  show (Conf []) = "<>"
  show (Conf (x : xs)) =
    let show_ []       = ""
        show_ (x : xs) = ", " ++ show x ++ show_ xs
     in "<" ++ show x ++ show_ xs ++ ">"

data Elem
  = Syntax String
  | Variable String

instance Show Elem where
  show (Syntax s)   = "\"" ++ s ++ "\""
  show (Variable x) = x

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

