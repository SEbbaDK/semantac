module Ast where

import           Data.List           (find)

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

termLookup :: String -> [Loc TermDecl] -> Maybe (Loc TermDecl)
termLookup name terms = find ((== name) . dName . unLoc) terms

data CategoryDecl
  = CategoryDecl
    { cName :: String
    , cType :: Type
    , cIn   :: Bool
    }
  deriving (Show, Eq, Ord)

categoryLookup :: String -> [Loc CategoryDecl] -> Maybe (Loc CategoryDecl)
categoryLookup name cats = find ((== name) . cName . unLoc) cats

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
    , literal  :: Bool
    , unused   :: Bool
    }
  deriving (Show)

instance Eq Variable where
    v1 == v2 = and
        [ varName v1 == varName v2
        , marks v1 == marks v2
        ]

instance Ord Variable where
    v1 <= v2 =
        varName v1 <= varName v2 ||
        marks v1 <= marks v2

    compare v1 v2 =
        case compare (varName v1) (varName v2) of
            EQ -> compare (marks v1) (marks v2)
            r -> r

rootVariable :: VariableExpr -> Variable
rootVariable (VRef v) = v
rootVariable (VBind v _ _) = rootVariable v

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

