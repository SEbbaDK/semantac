module Ast where

import           Loc
import           Types

data Specification
  = Specification
    { sTerms      :: [Loc TermDecl]
    , sCategories :: [Loc CategoryDecl]
    , sSystems    :: [Loc SystemDecl]
    , sRules      :: [Loc Rule]
    }
    deriving (Show)

data TermDecl
  = TermDecl
    { dName :: String
    , dType :: Type
    }
    deriving (Show)

data CategoryDecl
  = CategoryDecl
    { cName :: String
    , cType :: Type
    , cIn   :: Bool
    }
  deriving (Show)

data SystemDecl
  = SystemDecl
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
    { typeName :: String
    , varName  :: String
    , marks    :: Int
    , binds    :: [(Variable, Variable)]
    }
  deriving (Show)

instance Eq Variable where
    (==) (Variable t1 n1 m1 b1) (Variable t2 n2 m2 b2) =
        t1 == t2 && n1 == n2 && m1 == m2
instance Ord Variable where
    compare (Variable t1 n1 m1 b1) (Variable t2 n2 m2 b2) = let
        tc = compare t1 t2
        nc = compare n1 n2
        mc = compare m1 m2
      in
        if tc /= EQ
            then tc
            else if nc /= EQ
                then nc
                else mc
    (<=) v1 v2 =
        GT /= compare v1 v2

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

