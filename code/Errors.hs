{-# LANGUAGE NamedFieldPuns #-}
module Errors where

import           Ast
import           Data.List (intercalate)
import           Loc       (Loc (Loc), Pos, pos, showPos, showPosInSource)
import           Types     (Type (TVar), TypeVar)

newtype Error a
  = Error (ContextStack, a)

instance Functor Error where
  fmap f (Error (ctx, val)) = Error (ctx, f val)


data SpecificationError
  = CategoryError CategoryError
  | SystemError SystemError
  | RuleError String RuleError
  | MultiRuleDefinitions [Loc String]

-- Todo: figure out what errors you can have here
data CategoryError = CatError

-- Todo: figure out what errors you can have here
data SystemError = SysError

data RuleError
  = TypeMismatch (Loc Type) (Loc Type)
  -- I'm not sure if the InfiniteType error is possible given that all
  -- functions have to be explicitly declared upfront with their type
  -- signatures.
  | InifiniteType TypeVar (Loc Type)
  | UndefinedVar (Loc String)
  | UndefinedArrow (Loc String)
  | ConfTypeMismatch (Loc Type) (Loc Type) (Loc System)

type Lines = [String]

showErrorMessage :: String -> Error SpecificationError -> String
showErrorMessage src (Error (ctx, e)) = unlines (showTopErrorLines src e)

showErrorInSource :: Error a -> String -> String
showErrorInSource (Error (ctx, _)) src =
  let p = contextPos $ head ctx
  in showPosInSource p src

showTopErrorLines :: String -> SpecificationError -> Lines
showTopErrorLines src (CategoryError e)        = showCategoryError e
showTopErrorLines src (SystemError e)          = showSystemError e
showTopErrorLines src (RuleError ruleName e)   = showRuleError src e
showTopErrorLines src (MultiRuleDefinitions e) = ["todo"]

showCategoryError :: CategoryError -> Lines
showCategoryError _ = ["todo"]

showSystemError :: SystemError -> Lines
showSystemError _ = ["todo"]

showRuleError :: String -> RuleError -> Lines
showRuleError src (TypeMismatch (Loc p1 t1) (Loc p2 t2)) =
  [ header $ "Type mismatch between: " ++ showPos p1
  , showPosInSource p1 src
  , header $ "  and: " ++ showPos p2
  , showPosInSource p2 src
  , bold $ "  Expected: " ++ highlight (show t2)
  , bold $ "  Received: " ++ highlight (show t1)
  ]
showRuleError src (InifiniteType tv (Loc p t)) =
  -- This message is kinda impossible to understand I think.
  -- Failure of the "occurs check" is the terminology in the literature for this type of error.
  [ header $ "Infinite type. "
  , "  Type variable " ++ show (TVar tv) ++ " occurs in " ++ show t
  , showPosInSource p src
  ]
showRuleError src (UndefinedVar (Loc p name)) =
  [ header $ "Use of undefined variable \"" ++ name ++ "\""
  , showPosInSource p src
  ]
showRuleError src (UndefinedArrow (Loc p arrowName)) =
  [ header $ "Use of undefined arrow: " ++ arrowName
  , showPosInSource p src
  ]
showRuleError src (ConfTypeMismatch (Loc usedPos usedType) (Loc defPos defType) (Loc confpos sys)) =
  let
    System { arrow, initial, final } = sys
  in
    [ header $ "Mismatch between configuration and defined transition system."
    , ""
    , "The type of the configuration at " ++ showPos usedPos ++ " does not match the type given in the definition of the transition system: " ++ arrow
    , ""
    , bold $ "  The type of the configuration: " ++ highlight (show usedType)
    , showPosInSource usedPos src
    , bold $ "  The system specifies that it should be: " ++ highlight (show defType)
    , showPosInSource defPos src
    , bold $ "These two types should match, but they do not."
    ]

code c s = "\x1b[" ++ c ++ "m" ++ s ++ "\x1b[m"
bold = code "1"
underline = code "4"
highlight = code "32"
header = bold . (code "37:42")

data Context
  = CRule (Loc Rule)
  | CCategory (Loc Category)
  | CSystem (Loc System)
  | CSpec (Loc Type)
  | CPremise (Loc Premise)
  | CEquality (Loc Expr) (Loc Expr)
  | CInequality (Loc Expr) (Loc Expr)
  | CConclusion (Loc Transition)
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

