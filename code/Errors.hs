module Errors where

import Ast
import Types (Type (TVar), TypeVar)
import Data.List (intercalate)

data TopError
  = CategoryError CategoryError
  | SystemError SystemError
  | RuleError String RuleError
  | MultiRuleDefinitions [Loc String]

instance Show TopError where
  show (CategoryError e) = showCategoryError e
  show (SystemError e) = showSystemError e
  show (RuleError ruleName e) =
    let
      topLine = "in rule \"" ++ ruleName ++ "\":"
      lines = fmap ("  " ++) (showRuleError e)
    in
    intercalate
      "\n"
      (topLine : lines)
  show (MultiRuleDefinitions e) =
    "todo"

-- Todo: figure out what errors you can have here
data CategoryError = CatError

-- Todo: figure out what errors you can have here
data SystemError = SysError

data RuleError
  = PremiseError PremiseError
  | ConclusionError TransitionError
  | VarTypeMismatch Type Type
  | VarInifiniteType TypeVar Type -- occurs check fails

data PremiseError
  = TransitionError TransitionError
  | EqualityError EqualityError

-- Todo: figure out what errors you can have here
data EqualityError = EqError

data TransitionError
  = LeftUndefinedVar (Loc String)
  | RightUndefinedVar (Loc String)
  | UndefinedArrow String
  | ConfTypeMismatch (Loc Type) (Loc String) (Loc Type)

showCategoryError :: CategoryError -> String
showCategoryError _ = "todo"

showSystemError :: SystemError -> String
showSystemError _ = "todo"

showRuleError :: RuleError -> [String]
showRuleError (PremiseError e) = showPremiseError e
showRuleError (ConclusionError e) = error "todo"
showRuleError (VarTypeMismatch t1 t2) =
  [ "Type mismatch."
  , "  Expected " ++ show t2
  , "  Found    " ++ show t1
  ]
showRuleError (VarInifiniteType tv t) =
  [ "Infinite type. "
  , "  Type variable " ++ show (TVar tv) ++ " occurs in " ++ show t
  ]

showPremiseError :: PremiseError -> [String]
showPremiseError e = error "todo"


showConclusionError :: TransitionError -> [String]
showConclusionError e = error "todo"
