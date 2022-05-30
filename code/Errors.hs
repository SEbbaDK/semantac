{-# LANGUAGE NamedFieldPuns #-}
module Errors where

import           Ast
import           Graph
import           Data.List (intercalate)
import           Loc       (Loc (Loc), Pos, pos, showPos, showPosInSource)
import           Types     (Type (TVar), TypeVar)
import           Pretty

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
  | UndefinedTerm (Loc String)
  | UndefinedVar Node [Variable]
  | UnusedVar Node [Variable]
  | UnreachablePremise Node
  | UndefinedArrow (Loc String)
  | UndefinedType (Loc String)
  | ConfTypeMismatch (Loc Type) (Loc Type) (Loc SystemDecl)

type Lines = [String]

showErrorMessage :: String -> Error SpecificationError -> String
showErrorMessage src (Error (ctx, e)) = unlines (showSpecError src e)

showErrorInSource :: Error a -> String -> String
showErrorInSource (Error (ctx, _)) src =
    let p = contextPos $ head ctx
    in showPosInSource p src

showSpecError :: String -> SpecificationError -> Lines
showSpecError src (CategoryError e)        = showCategoryError e
showSpecError src (SystemError e)          = showSystemError e
showSpecError src (RuleError ruleName e)   = showRuleError src e
showSpecError src (MultiRuleDefinitions e) = ["todo"]

showCategoryError :: CategoryError -> Lines
showCategoryError _ = ["todo"]

showSystemError :: SystemError -> Lines
showSystemError _ = ["todo"]

showRuleError :: String -> RuleError -> Lines
showRuleError src err = case err of
    TypeMismatch (Loc p1 t1) (Loc p2 t2) ->
        [ header $ "Type mismatch between: " ++ showPos p1
        , showPosInSource p1 src
        , header $ "  and: " ++ showPos p2
        , showPosInSource p2 src
        , bold $ "  Expected: " ++ highlight (show t2)
        , bold $ "  Received: " ++ highlight (show t1)
        ]
    InifiniteType tv (Loc p t) ->
        -- This message is kinda impossible to understand I think.
        -- Failure of the "occurs check" is the terminology in the literature for this type of error.
        [ header $ "Infinite type. "
        , "  Type variable " ++ show (TVar tv) ++ " occurs in " ++ show t
        , showPosInSource p src
        ]
    UndefinedTerm (Loc p name) ->
        [ header $ "Use of undefined variable \"" ++ name ++ "\""
        , showPosInSource p src
        ]
    UndefinedVar node vars ->
        [ header $ "Unbound variables in " ++ nodeExplanation node
        , "The unbound variables are: [ " ++ (unwords $ map pprint vars) ++ " ]"
        , showPosInSource (nodePos node) src
        ]
    UnusedVar node vars ->
        [ header $ "Unused variables in " ++ nodeExplanation node
        , "The variables not being used are: [ " ++ (unwords $ map pprint vars) ++ " ]"
        , showPosInSource (nodePos node) src
        ]
    UnreachablePremise node ->
        [ header $ nodeExplanation node ++ " is not reachable via any variables."
        , "None of the variables in the premise are used in rules required to reach the conclusion or reachable from the initial configuration."
        , showPosInSource (nodePos node) src
        ]
    UndefinedArrow (Loc p arrowName) ->
        [ header $ "Use of undefined arrow: " ++ arrowName
        , showPosInSource p src
        ]
    UndefinedType (Loc p ty) ->
        [ header $ "Use of undefined type \"" ++ ty ++ "\""
        , showPosInSource p src
        ]
    ConfTypeMismatch (Loc usedPos usedType) (Loc defPos defType) (Loc confpos sys) ->
        [ header $ "Mismatch between configuration and defined transition system."
        , ""
        , "The type of the configuration at " ++ showPos usedPos ++ " does not match the type given in the definition of the transition system: " ++ arrow sys
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
header = bold . code "37:42"

data Context
  = CRule (Loc Rule)
  | CCategory (Loc CategoryDecl)
  | CSystem (Loc SystemDecl)
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

nodeExplanation :: Node -> String
nodeExplanation (InitNode _) = "the initial configuration of the rules conclusion"
nodeExplanation (ConcNode _) = "the final configuration of the rules conclusion"
nodeExplanation (PremNode _) = "a premise of the rule"

contextName :: Context -> String
contextName ctx = case ctx of
    CRule node                    -> "Rule \"" ++ name ++ "\""
        where (Loc loc Rule { name }) = node
    CCategory node                -> "Category Definition"
    CSystem node                  -> "System Definition"
    CSpec node                    -> "Type Specification"
    CPremise node                 -> "Premise"
    CEquality left right          -> "Equality"
    CInequality left right        -> "Inequality"
    CConclusion node              -> "Conclusion"
    CConf node                    -> "Configuration"
    CConfSyntaxList node          -> "ConfSyntaxList"
    CConfBinding left arrow right -> "ConfBinding"

contextPos :: Context -> Pos
contextPos ctx = case ctx of
    CRule (Loc l _)                      -> l
    CCategory (Loc l _)                  -> l
    CSystem (Loc l _)                    -> l
    CSpec (Loc l _)                      -> l
    CPremise (Loc l _)                   -> l
    CEquality (Loc l1 _) (Loc l2 _)      -> l1 -- TODO: merge l1 and l2
    CInequality (Loc l1 _) (Loc l2 _)    -> l1
    CConclusion (Loc l _)                -> l
    CConf (Loc l _)                      -> l
    CConfSyntaxList []                   -> error "todo"
    CConfSyntaxList ((Loc l _) : xs)     -> l
    CConfBinding (Loc l1 _) _ (Loc l2 _) -> l1

