{-# LANGUAGE NamedFieldPuns #-}

module Ast where

data Top = Top [Domain] [Rule]

data Domain = Domain
    { domain :: String
    , spec :: Spec
    }

data Spec = Integer | Identifier | Cross [Spec] | Union [Spec]

data Rule = Rule
    { name :: String
    , base :: Trans
    , premises :: [Premise]
    , properties :: [Property]
    }

data Property = Ambiguous | Nonterminating

data Trans = Trans
    { system :: String
    , before :: Conf
    , after  :: Conf
    }

type Conf = [Elem]
data Elem = Syntax | Variable String

data Premise = TPremise Trans | TEquality Equality
data Equality = Eq Expr Expr | InEq Expr Expr
type Expr = String

