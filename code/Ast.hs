module Ast where

import           Loc
import           Types

data Specification
  = Specification [Loc Declaration] [Loc Category] [Loc System] [Loc Rule]
  deriving (Show)

data Declaration
  = Declaration String Type
  deriving (Show)

data Category
  = Category
    { cName    :: String
    , variable :: String
    , cType    :: Type
    }
  deriving (Show)

data System
  = System
    { arrow   :: String
    , initial :: Loc Type
    , final   :: Loc Type
    }
  deriving (Show)

data Rule
  = Rule
    { name       :: String
    , base       :: Transition
    , premises   :: [Premise]
    , properties :: [Property]
    }
  deriving (Show)

data Property = NonDeterministic | NonTerminating
  deriving (Show)

data Transition
  = Transition
    { system :: String
    , before :: Loc Conf
    , after  :: Loc Conf
    }
  deriving (Show)

data Variable
  = Variable
    { typename :: String
    , varname  :: String
    , marks    :: Int
    , binds    :: [(Variable, Variable)]
    }
  deriving (Show)

data Conf
  = Conf [Loc Conf]
  | Syntax String
  | Var Variable
  | SyntaxList [Loc Conf]
  | Paren (Loc Conf)
  deriving (Show)

data Premise
  = PTransition Transition
  | PEquality Equality
  | PDefinition Expr Expr
  deriving (Show)

data Equality
  = Eq Expr Expr
  | InEq Expr Expr
  deriving (Show)

data Expr
  = EVar Variable
  | ECall Expr [Expr]
  deriving (Show)

