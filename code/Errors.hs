{-# LANGUAGE NamedFieldPuns #-}
module Errors where

import           Ast
import           Data.List (intercalate)
import           Loc       (Loc (Loc), Pos, showLocInSource, showPos)
import           Types     (Type (TVar), TypeVar)

newtype Error a
  = Error (ContextStack, a)

instance Functor Error where
  fmap f (Error (ctx, val)) = Error (ctx, f val)


data TopError
  = CategoryError CategoryError
  | SystemError SystemError
  | RuleError String RuleError
  | MultiRuleDefinitions [Loc String]

-- Todo: figure out what errors you can have here
data CategoryError = CatError

-- Todo: figure out what errors you can have here
data SystemError = SysError

data RuleError
  = TypeMismatch Type (Loc Type)
  | InifiniteType TypeVar (Loc Type)
  | UndefinedVar (Loc String)
  | UndefinedArrow (Loc String)
  | ConfTypeMismatch (Loc Type) (Loc String) (Loc Type)

type Lines = [String]

showErrorMessage :: Error TopError -> String
showErrorMessage (Error (ctx, e)) = intercalate "\n" (showTopErrorLines e)

showTopErrorLines :: TopError -> Lines
showTopErrorLines (CategoryError e)        = showCategoryError e
showTopErrorLines (SystemError e)          = showSystemError e
showTopErrorLines (RuleError ruleName e)   = showRuleError e
showTopErrorLines (MultiRuleDefinitions e) = ["todo"]

showCategoryError :: CategoryError -> Lines
showCategoryError _ = ["todo"]

showSystemError :: SystemError -> Lines
showSystemError _ = ["todo"]

showRuleError :: RuleError -> Lines
showRuleError (TypeMismatch t1 t2) =
  [ "Type mismatch at " ++ showLocInSource t2 ""
  , "  Expected " ++ show t2
  , "  Found    " ++ show t1
  ]
showRuleError (InifiniteType tv t) =
  -- This message is kinda impossible to understand I think.
  -- Failure of the "occurs check" is the terminology in the literature for this type of error.
  [ "Infinite type. "
  , "  Type variable " ++ show (TVar tv) ++ " occurs in " ++ show t
  ]
showRuleError (UndefinedVar (Loc _ name)) =
  [ "Use of undefined variable \"" ++ name ++ "\""
  ]
showRuleError (UndefinedArrow (Loc _ arrowName)) =
  [ "Use of undefined arrow: " ++ arrowName
  ]
showRuleError (ConfTypeMismatch left arrow right) =
  [ "The type of the configuration does not match the type of the transition used."
  , "  Expected TODO"
  , "  Found    " ++ show left ++ " " ++ show arrow ++ " " ++ show right
  ]


data Context
  = CRule (Loc Rule)
  | CCategory (Loc Category)
  | CSystem (Loc System)
  | CSpec (Loc Spec)
  | CPremise (Loc Premise)
  | CEquality (Loc Expr) (Loc Expr)
  | CInequality (Loc Expr) (Loc Expr)
  | CConclusion (Loc Trans)
  | CConf (Loc Conf)
  | CConfSyntaxList [Loc Conf]
  | CConfBinding (Loc Conf) (Loc Conf) (Loc Conf)

type ContextStack = [Context]

showStackTrace :: Error a -> [String]
showStackTrace (Error (ctx, e)) = showStackTrace_ ctx

showStackTrace_ :: ContextStack -> Lines
showStackTrace_ stack =
  let
    names = map contextName stack
    locs = map contextLoc stack

    maxLeftLen = maximum (map length names)

    combine :: (String, Pos) -> String
    combine (name, pos) =
      let padding = maxLeftLen - length name + 4 in
      name ++ concat (replicate padding " ") ++ "[" ++ showPos pos ++ "]"
  in
  fmap combine (zip names locs)

contextName :: Context -> String
contextName (CRule node)                    = "Rule \"" ++ name ++ "\""
  where (Loc loc Rule { name }) = node
contextName (CCategory node)                = "Category"
contextName (CSystem node)                  = "System"
contextName (CSpec node)                    = "Spec"
contextName (CPremise node)                 = "Premise"
contextName (CEquality left right)          = "Equality"
contextName (CInequality left right)        = "Inequality"
contextName (CConclusion node)              = "Conclusion"
contextName (CConf node)                    = "Conf"
contextName (CConfSyntaxList node)          = "ConfSyntaxList"
contextName (CConfBinding left arrow right) = "ConfBinding"

contextLoc :: Context -> Pos
contextLoc (CRule (Loc l _))                      = l
contextLoc (CCategory (Loc l _))                  = l
contextLoc (CSystem (Loc l _))                    = l
contextLoc (CSpec (Loc l _))                      = l
contextLoc (CPremise (Loc l _))                   = l
contextLoc (CEquality (Loc l1 _) (Loc l2 _))      = l1 -- TODO: merge l1 and l2
contextLoc (CInequality (Loc l1 _) (Loc l2 _))    = l1
contextLoc (CConclusion (Loc l _))                = l
contextLoc (CConf (Loc l _))                      = l
contextLoc (CConfSyntaxList [])                   = error "todo"
contextLoc (CConfSyntaxList ((Loc l _) : xs))     = l
contextLoc (CConfBinding (Loc l1 _) _ (Loc l2 _)) = l1

