{-# LANGUAGE NamedFieldPuns #-}
module Errors where

import           Ast
import           Data.List (intercalate)
import           Graph
import           Loc       (Loc (Loc), Pos, pos, showPos, showPosInSource, unLoc)
import           Pretty
import           Types     (Type (TVar), TypeVar)

newtype Error a
  = Error (ContextStack, a)

unErr (Error (_, a)) = a

instance Functor Error where
    fmap f (Error (ctx, val)) = Error (ctx, f val)


data SpecificationError
  = DefinitionError DefinitionError
  | RuleError (Loc Rule) RuleError

data DefinitionError
    = OverlappingTerms [Loc TermDecl]
    | OverlappingCategories [Loc CategoryDecl]
    | OverlappingSystems [Loc SystemDecl]
    | OverlappingRules [Loc Rule]

data RuleError
  = TypeMismatch (Loc Type) (Loc Type)
  -- I'm not sure if the InfiniteType error is possible given that all
  -- functions have to be explicitly declared upfront with their type
  -- signatures.
  | InfiniteType TypeVar (Loc Type)
  | UndefinedTerm (Loc String)
  | UndefinedVar Node [Variable]
  | UnusedVar Node [Variable]
  | UseUnusableVar Node [Variable]
  | MultidefinedVar Variable [Node]
  | VariableTermOverlap Node Variable (Loc TermDecl)
  | UnreachablePremise Node
  | UndefinedArrow (Loc String)
  | UndefinedType (Loc String)
  | ConfTypeMismatch (Loc Type) (Loc Type) (Loc SystemDecl)

type Lines = [String]

showError :: String -> Error SpecificationError -> String
showError src e
    = showErrorMessage src (unErr e) ++
      concatMap ("  in " ++) (showStackTrace e)

showErrorMessage :: String -> SpecificationError -> String
showErrorMessage src e = unlines (showSpecError src e)

showSpecError :: String -> SpecificationError -> Lines
showSpecError src (DefinitionError e)      = showDefinitionError src e
showSpecError src (RuleError ruleName e)   = showRuleError src e

showDefinitionError src err = case err of
    OverlappingTerms o -> overlapGenerator "term" dName src o
    OverlappingCategories o -> overlapGenerator "category" cName src o
    OverlappingSystems o -> overlapGenerator "system" arrow src o
    OverlappingRules o -> overlapGenerator "rule" name src o
    where
        overlapGenerator name access src overlaps =
            [ header $ "Overlapping " ++ name ++ " declarations."
            , "The " ++ name ++ " '" ++ (access $ unLoc $ head overlaps) ++
                "' is declared multiple times."
            , concat $ map (\t -> "\n" ++ concat
                [ "  Definition at " ++ showPos (pos t)
                , showPosInSource (pos t) src
                ]) overlaps
            ]

showRuleError :: String -> RuleError -> Lines
showRuleError src err = case err of
    TypeMismatch (Loc p1 t1) (Loc p2 t2) ->
        [ header $ "Type mismatch"
        , "  Type at " ++ showPos p1
        , showPosInSource p1 src
        , "  and " ++ showPos p2
        , showPosInSource p2 src
        , bold $ "  Expected: " ++ highlight (pprint t2)
        , bold $ "  Received: " ++ highlight (pprint t1)
        , ""
        ]
    InfiniteType tv (Loc p t) ->
        -- This message is kinda impossible to understand I think.
        -- Failure of the "occurs check" is the terminology in the literature for this type of error.
        [ header $ "Infinite type"
        , "  Type variable " ++ pprint (TVar tv) ++ " occurs in " ++ pprint t
        , showPosInSource p src
        ]
    UndefinedTerm (Loc p name) ->
        [ header $ "Use of undefined variable \"" ++ name ++ "\""
        , showPosInSource p src
        ]
    UndefinedVar node vars ->
        [ header $ "Unbound variables in " ++ nodeExplanation node ++ "."
        , "The unbound variables are: [ " ++ (unwords $ map pprint vars) ++ " ]"
        , showPosInSource (nodePos node) src
        ]
    UnusedVar node vars ->
        [ header $ "Unused variables in " ++ nodeExplanation node ++ "."
        , "The variables not being used are: [ " ++ (unwords $ map pprint vars) ++ " ]"
        , showPosInSource (nodePos node) src
        ]
    UseUnusableVar node vars ->
        [ header $ "Node uses a variable that is marked as unused"
        , "The variables marked as unused are: [ " ++ (unwords $ map pprint vars) ++ " ] and they're used in " ++ nodeExplanation node ++ "."
        , ""
        , "Nodes that start with an underscore (_a or _) shouldn't be used in places like expressions and final configurations, because their use is supposed to mark a variable that will not be used. Use them only in defining configurations (ie. <a,_e> -> <a>)."
        , showPosInSource (nodePos node) src
        ]
    MultidefinedVar var nodes ->
        [ header $ "Multiple definitions of the variable " ++ pprint var
        , "  The variable " ++ pprint var ++ " is defined in multiple places:"
        ] ++ map nodeExpl nodes
          where
            nodeExpl node = intercalate "\n"
                [ "  • Definition in " ++ nodeExplanation node ++ "."
                , showPosInSource (nodePos node) src
                ]
    VariableTermOverlap node var (Loc tp term) ->
        [ header $ "Variable name overlaps with defined term."
        , "The variable " ++ pprint var ++ " overlaps with the term " ++ dName term ++ "."
        , ""
        , "Variable used at:"
        , showPosInSource (nodePos node) src
        , "Term definition:"
        , showPosInSource tp src
        , "Variables are not allowed to 'hide' the built-in terms, as that might cause confusion to the reader and another name should be used instead"
        , ""
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
        , "The type of the configuration at " ++ showPos usedPos ++ " does not match the type given in the definition of the transition system: " ++ arrow sys
        , ""
        , bold $ "  The type of the configuration: " ++ highlight (pprint usedType)
        , showPosInSource usedPos src
        , bold $ "  The system specifies that it should be: " ++ highlight (pprint defType)
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

