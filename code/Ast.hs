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

data VariableExpr
  = VRef Variable
  | VBind VariableExpr Variable Expr
  deriving (Show)

data Variable
  = Variable
    { typeName :: Maybe String
    , varName  :: String
    , marks    :: Int
    }
  deriving (Show, Eq)

instance Ord Variable where
    compare (Variable t1 n1 m1) (Variable t2 n2 m2) = let
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
  | Var VariableExpr
  | SyntaxList [Loc Conf]
  | Paren (Loc Conf)
  deriving (Show)

data Premise
  = PTransition Transition
  | PConstraint Expr
  | PDefinition Expr Expr
  deriving (Show)

data Expr
  = EVar VariableExpr
  | ECall Expr [Expr]
  | EEq Expr Expr
  | EInEq Expr Expr
  deriving (Show)

