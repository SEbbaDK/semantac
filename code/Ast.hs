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
    deriving (Show, Eq, Ord)

data TermDecl
  = TermDecl
    { dName :: String
    , dType :: Type
    }
    deriving (Show, Eq, Ord)

data CategoryDecl
  = CategoryDecl
    { cName :: String
    , cType :: Type
    , cIn   :: Bool
    }
  deriving (Show, Eq, Ord)

data SystemDecl
  = SystemDecl
    { arrow   :: String
    , initial :: Loc Type
    , final   :: Loc Type
    }
  deriving (Show, Eq, Ord)

data Rule
  = Rule
    { name       :: String
    , base       :: Loc Transition
    , premises   :: [Loc Premise]
    , properties :: [Property]
    }
  deriving (Show, Eq, Ord)

data Property = NonDeterministic | NonTerminating
  deriving (Show, Eq, Ord)

data Transition
  = Transition
    { system :: String
    , before :: Loc Conf
    , after  :: Loc Conf
    }
  deriving (Show, Eq, Ord)

data VariableExpr
  = VRef Variable
  | VBind VariableExpr Variable Expr
  deriving (Show, Eq, Ord)

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

data Expr
  = EVar VariableExpr
  | ECall Expr [Expr]
  | EEq Expr Expr
  | EInEq Expr Expr
  deriving (Show, Eq, Ord)

-- SYNTAX

data Conf = Conf [SyntaxList]
  deriving (Show, Eq, Ord)

type SyntaxList = [Loc SyntaxElem]
data SyntaxElem
  = Syntax String
  | Var VariableExpr
  | SubElem SyntaxList
  deriving (Show, Eq, Ord)

data Premise
  = PTransition Transition
  | PConstraint Expr
  | PDefinition Expr Expr
  deriving (Show, Eq, Ord)

