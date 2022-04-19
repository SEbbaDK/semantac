{-# LANGUAGE NamedFieldPuns #-}
module Errors where

import           Ast
import           Data.List (intercalate)
import           Loc       (Loc (Loc), Pos, showPosInSource, showPos, pos)
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
  | ConfTypeMismatch (Loc Type) (Loc System)

type Lines = [String]

showErrorMessage :: String -> Error TopError -> String
showErrorMessage src (Error (ctx, e)) = unlines (showTopErrorLines src e)

showErrorInSource (Error (ctx, _)) src =
  let p = contextPos $ head ctx
  in showPosInSource p src

showTopErrorLines :: String -> TopError -> Lines
showTopErrorLines src (CategoryError e)        = showCategoryError e
showTopErrorLines src (SystemError e)          = showSystemError e
showTopErrorLines src (RuleError ruleName e)   = showRuleError src e
showTopErrorLines src (MultiRuleDefinitions e) = ["todo"]

showCategoryError :: CategoryError -> Lines
showCategoryError _ = ["todo"]

showSystemError :: SystemError -> Lines
showSystemError _ = ["todo"]

showRuleError :: String -> RuleError -> Lines
showRuleError src (TypeMismatch t1 (Loc p t2)) =
  [ "Type mismatch at " ++ showPos p
  , showPosInSource p src
  , "  Expected: " ++ show t2
  , "  Received: " ++ show t1
  ]
showRuleError src (InifiniteType tv (Loc p t)) =
  -- This message is kinda impossible to understand I think.
  -- Failure of the "occurs check" is the terminology in the literature for this type of error.
  [ "Infinite type. "
  , "  Type variable " ++ show (TVar tv) ++ " occurs in " ++ show t
  , showPosInSource p src
  ]
showRuleError src (UndefinedVar (Loc p name)) =
  [ "Use of undefined variable \"" ++ name ++ "\""
  , showPosInSource p src
  ]
showRuleError src (UndefinedArrow (Loc p arrowName)) =
  [ "Use of undefined arrow: " ++ arrowName
  , showPosInSource p src
  ]
showRuleError src (ConfTypeMismatch (Loc usedPos used) (Loc sysPos sys)) =
  let
    System { arrow, initial, final } = sys
  in
    [ "The type of the configuration at " ++ showPos usedPos ++ " does not match the type given in the definition of the transition system.\n"
    , "  The type of the configuration is: " ++ show used
    , showPosInSource usedPos src
    , "  System specification: " ++ show sys
    , showPosInSource sysPos src
    , "These two types should match, but they do not.\n"
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
    locs = map contextPos stack

    maxLeftLen = maximum (map length names)

    combine :: (String, Pos) -> String
    combine (name, pos) =
      let padding = maxLeftLen - length name + 4 in
      name ++ concat (replicate padding " ") ++ "[" ++ showPos pos ++ "]\n"
  in
  fmap combine (zip names locs)

contextName :: Context -> String
contextName (CRule node)                    = "Rule \"" ++ name ++ "\""
  where (Loc loc Rule { name }) = node
contextName (CCategory node)                = "Category Definition"
contextName (CSystem node)                  = "System Definition"
contextName (CSpec node)                    = "Type Specification"
contextName (CPremise node)                 = "Premise"
contextName (CEquality left right)          = "Equality"
contextName (CInequality left right)        = "Inequality"
contextName (CConclusion node)              = "Conclusion"
contextName (CConf node)                    = "Configuration"
contextName (CConfSyntaxList node)          = "ConfSyntaxList"
contextName (CConfBinding left arrow right) = "ConfBinding"

contextPos :: Context -> Pos
contextPos (CRule (Loc l _))                      = l
contextPos (CCategory (Loc l _))                  = l
contextPos (CSystem (Loc l _))                    = l
contextPos (CSpec (Loc l _))                      = l
contextPos (CPremise (Loc l _))                   = l
contextPos (CEquality (Loc l1 _) (Loc l2 _))      = l1 -- TODO: merge l1 and l2
contextPos (CInequality (Loc l1 _) (Loc l2 _))    = l1
contextPos (CConclusion (Loc l _))                = l
contextPos (CConf (Loc l _))                      = l
contextPos (CConfSyntaxList [])                   = error "todo"
contextPos (CConfSyntaxList ((Loc l _) : xs))     = l
contextPos (CConfBinding (Loc l1 _) _ (Loc l2 _)) = l1

