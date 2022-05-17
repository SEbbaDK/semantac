{-# LANGUAGE NamedFieldPuns #-}

module Ast where

import           Data.List (intercalate, intersperse)
import           Loc
import           Types

data Specification
  = Specification [Loc Declaration] [Loc Category] [Loc System] [Loc Rule]

data Declaration
  = Declaration { fName :: String, fType :: Type}

data Category
  = Category
    { cName    :: String
    , variable :: String
    , cType    :: Type
    }

data System
  = System
    { arrow   :: String
    , initial :: Loc Type
    , final   :: Loc Type
    }

instance Show Specification where
  show (Specification declarations categories systems rules) =
    unlines lines
    where
      lines = intercalate [""]
        [ map show declarations
        , map show categories
        , map show systems
        , intersperse "" (map show rules)
        ]

instance Show Declaration where
  show (Declaration name spec) =
    unwords [ "meta", name, "=", show spec ]

instance Show Category where
  show Category {cName, variable, cType} =
    unwords [ "category", variable, "in", cName, ":", show cType ]

instance Show System where
  show System {arrow, initial, final} =
    unwords [ "system", show initial, arrow, show final ]

data Rule
  = Rule
    { name       :: String
    , base       :: Transition
    , premises   :: [Premise]
    , properties :: [Property]
    }

instance Show Rule where
  show Rule {name, base, premises, properties} =
    unlines lines
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

data Transition
  = Transition
    { system :: String
    , before :: Loc Conf
    , after  :: Loc Conf
    }

instance Show Transition where
  show Transition { system, before, after } =
    show before ++ " " ++ system ++ " " ++ show after

data Variable
  = Variable
    { typeName :: String
    , varName  :: String
    , binds    :: [(Variable, Variable)]
    }
    deriving (Eq, Ord)

varNamer :: String -> String
varNamer ""  = ""
varNamer "0" = "₀"
varNamer "1" = "₁"
varNamer "2" = "₂"
varNamer "3" = "₃"
varNamer "4" = "₄"
varNamer "5" = "₅"
varNamer "6" = "₆"
varNamer "7" = "₇"
varNamer "8" = "₈"
varNamer "9" = "₉"
varNamer nam = "_" ++ nam

instance Show Variable where
  show Variable { typeName, varName, binds } = concat
    [ typeName
    , varNamer varName
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
  = PTransition Transition
  | PEquality Equality

instance Show Premise where
  show (PTransition trans) = show trans
  show (PEquality eq)      = show eq

data Equality
  = Eq Expr Expr
  | InEq Expr Expr

instance Show Equality where
  show (Eq l r)   = show l ++ " = " ++ show r
  show (InEq l r) = show l ++ " ≠ " ++ show r

data Expr
  = EVar Variable
  | ECall String [Expr]

instance Show Expr where
  show (EVar s)    = show s
  show (ECall o e) = o ++ "(" ++ intercalate ", " (map show e) ++ ")"

