module Errors where

import           Ast
import           Loc
import           Data.List (intercalate)
import           Types     (Type (TVar), TypeVar)

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
  = VarTypeMismatch Type Type
  | VarInifiniteType TypeVar Type
  | UndefinedVar (Loc String)
  | UndefinedArrow String
  | ConfTypeMismatch (Loc Type) (Loc String) (Loc Type)

type Lines = [String]

showCategoryError :: CategoryError -> String
showCategoryError _ = "todo"

showSystemError :: SystemError -> String
showSystemError _ = "todo"

showRuleError :: RuleError -> Lines
showRuleError (VarTypeMismatch t1 t2) =
  [ "Type mismatch."
  , "  Expected " ++ show t2
  , "  Found    " ++ show t1
  ]
showRuleError (VarInifiniteType tv t) =
  -- This message is kinda impossible to understand I think.
  -- Failure of the "occurs check" is the terminology in the literature for this type of error.
  [ "Infinite type. "
  , "  Type variable " ++ show (TVar tv) ++ " occurs in " ++ show t
  ]
showRuleError (UndefinedVar (Loc _ name)) =
  [ "Use of undefined variable \"" ++ name ++ "\""
  ]
showRuleError (UndefinedArrow arrowName) =
  [ "Use of undefined arrow: " ++ arrowName
  ]
showRuleError (ConfTypeMismatch left arrow right) =
  [ "The type of the configuration does not match the type of the transition used."
  , "  Expected TODO"
  , "  Found    " ++ show left ++ " " ++ show arrow ++ " " ++ show right
  ]
