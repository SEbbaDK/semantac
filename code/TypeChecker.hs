{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module TypeChecker where

import           Ast
import           Control.Monad       (foldM, void)
import           Control.Monad.State (MonadState (get, put), State, evalState)
import           Data.List           as List (intercalate)
import           Data.Map.Strict     as Map (Map, insert, lookup)
import           Data.Maybe          (fromMaybe)
import           Debug.Trace         (trace)

{-
What can we check?
- Transitions are consistent with their systems.
- Variables in the conclusion must be defined.
- Variables must not conflict with eachother.
- Syntax nodes must be consistent with the grammar.
-}


type CheckResult = Either UnifyError ()
type UnifyResult = Either UnifyError Type

-- How do we add context to the errors?
data UnifyError
  = Mismatch Type Type
  | UndefinedVar String
  | UndefinedArrow String
  | InfiniteType TypeVar Type

instance Show UnifyError where
    show (Mismatch t1 t2) = "Type mismatch. Expected " ++ show t2 ++ " but found " ++ show t1
    show (UndefinedVar name) = "Unbound variable \"" ++ name ++ "\""
    show (UndefinedArrow name) = "Unbound arrow \"" ++ name ++ "\""
    show (InfiniteType tv t) = "Infinite type. " ++ show (TVar tv) ++ " = " ++ show t

check :: Top -> CheckResult
check (Top domains systems rules) =
    let
        init = Right ()

        f :: CheckResult -> Rule -> CheckResult
        f (Right ()) rule = checkRule domains systems rule
        f (Left e) rule   = Left e
    in
    foldl f init rules

checkRule :: [Category] -> [System] -> Rule -> CheckResult
checkRule domains systems Rule {base, premises, properties} =
    let
        tEnv = newTypeEnv domains systems

        init = Right ()

        f :: CheckResult -> Premise -> State TypeEnv CheckResult
        f (Right ()) prem = checkPremiseSystems prem
        f (Left e) prem   = return (Left e)
    in
    evalState (foldM f init premises) tEnv

-- Premise matches system
checkPremiseSystems :: Premise -> State TypeEnv CheckResult
checkPremiseSystems (TPremise trans) = do
    let Trans { system } = trans
    sys <- getSystem system
    case sys of
        Nothing  -> return (Left (UndefinedArrow system))
        Just sys -> do
            res <- checkTransSystem sys trans
            return (void res)
checkPremiseSystems (TEquality eq) =
    return (trace "todo: eq" (Right ()))

-- Transition matches system
checkTransSystem :: System -> Trans -> State TypeEnv UnifyResult
checkTransSystem System { arrow, initial, final } Trans { system, before, after } = do
    t1 <- matches before initial
    case t1 of
        Left e -> return (Left e)
        Right t1 -> do
            t2 <- matches after final
            case t2 of
                Left e   -> return (Left e)
                Right t2 -> unify t1 t2

matches :: Conf -> Spec -> State TypeEnv UnifyResult
matches c s = do
    t1 <- infer c
    let t2 = fromSpec s
    state <- get
    unify t1 t2


newtype TypeVar
  = TypeVar Int
  deriving (Eq, Ord)

instance Show TypeVar where
    show (TypeVar v) = "~" ++ reverse (intToAlphaRev v)
        where
            intToAlphaRev n | n <= 0 = []
            intToAlphaRev n =
                let (q, r) = quotRem (n - 1) 26 in
                toEnum (fromEnum 'a' + r) : intToAlphaRev q


data Type
  = TInteger
  | TIdentifier
  | TSyntax
  | TCustom String
  | TCross [Type]
  | TUnion [Type]
  | TVar TypeVar
  deriving (Eq, Ord)

instance Show Type where
    show TInteger       = "Int"
    show TIdentifier    = "Id"
    show TSyntax        = "Syntax"
    show (TCustom name) = name
    show (TCross xs)    = "<" ++ intercalate " , " (fmap show xs) ++ ">"
    show (TUnion xs)    = "(" ++ intercalate " | " (fmap show xs) ++ ")"
    show (TVar tv)      =  show tv

fromSpec :: Spec -> Type
fromSpec Integer    = TInteger
fromSpec Identifier = TIdentifier
fromSpec SSyntax    = TSyntax
fromSpec (Custom x) = TCustom x
fromSpec (Cross xs) = TCross (fmap fromSpec xs)
fromSpec (Union xs) = TUnion (fmap fromSpec xs)

type Substitutions = Map TypeVar Type

subst :: Substitutions -> Type -> Type
subst subs TInteger    = TInteger
subst subs TIdentifier = TIdentifier
subst subs TSyntax     = TSyntax
subst subs (TCustom x) = TCustom x
subst subs (TCross xs) = TCross (fmap (subst subs) xs)
subst subs (TUnion xs) = TUnion (fmap (subst subs) xs)
subst subs (TVar tv)   = fromMaybe (TVar tv) (Map.lookup tv subs)


data TypeEnv
  = TypeEnv
    { nextTypeVar_ :: TypeVar
    , subs         :: Substitutions
    , bindings     :: Map String Type
    , domains      :: [Category]
    , systems      :: [System]
    }

newTypeEnv :: [Category] -> [System] -> TypeEnv
newTypeEnv domains systems = TypeEnv
    { nextTypeVar_ = TypeVar 1
    , subs = mempty
    , bindings = mempty
    , domains
    , systems
    }

getSystem :: String -> State TypeEnv (Maybe System)
getSystem systemArrow = do
    TypeEnv { systems } <- get
    case filter (\System {arrow } -> arrow == systemArrow) systems of
        []      -> return Nothing
        sys : _ -> return (Just sys)

nextTypeVar :: State TypeEnv TypeVar
nextTypeVar = do
    tEnv <- get
    let TypeEnv { nextTypeVar_ = TypeVar tv } = tEnv
    put tEnv { nextTypeVar_ = TypeVar (tv + 1) }
    return (TypeVar tv)


addBind :: String -> Type -> State TypeEnv ()
addBind name t = do
    tEnv <- get
    let TypeEnv { bindings } = tEnv
    put tEnv { bindings = Map.insert name t bindings }

lookupBind :: String -> State TypeEnv (Maybe Type)
lookupBind name = do
    TypeEnv { bindings } <- get
    return (Map.lookup name bindings)


applySubst :: State TypeEnv ()
applySubst = do
    tEnv <- get
    let TypeEnv { bindings, subs } = tEnv
    put tEnv { bindings = fmap (subst subs) bindings }


freeTypeVars :: Type -> [TypeVar]
freeTypeVars TInteger       = []
freeTypeVars TIdentifier    = []
freeTypeVars TSyntax        = []
freeTypeVars (TCustom name) = []
freeTypeVars (TCross xs)    = concatMap freeTypeVars xs
freeTypeVars (TUnion xs)    = concatMap freeTypeVars xs
freeTypeVars (TVar v)       = [v]

varBind :: TypeVar -> Type -> State TypeEnv UnifyResult
varBind tv t =
    if tv `elem` freeTypeVars t then
        return (Left (InfiniteType tv t))
    else do
        tEnv <- get
        let TypeEnv { subs } = tEnv
        put tEnv { subs = Map.insert tv t subs }
        return (Right t)


unify :: Type -> Type -> State TypeEnv UnifyResult
unify TInteger TInteger =
    return (Right TInteger)
unify TSyntax TSyntax =
    return (Right TSyntax)
unify (TCustom x1) (TCustom x2) | x1 == x2  =
    return (Right (TCustom x1))
unify t (TVar tv) =
    varBind tv t
unify (TVar tv) t =
    varBind tv t
unify (TCross t1) (TCross t2) | length t1 == length t2  =
    unifyCross (zip t1 t2) []
unify t1 t2 =
    return (Left (Mismatch t1 t2))

unifyCross :: [(Type, Type)] -> [Type] -> State TypeEnv UnifyResult
unifyCross [] results =
    (return . Right . TCross . reverse) results
unifyCross ((t1_, t2_) : ts_) results = do
    r <- unify t1_ t2_
    case r of
        Right t -> unifyCross ts_ (t : results)
        Left e  -> return (Left e)


infer :: Conf -> State TypeEnv Type
infer (Syntax _) =
    return TSyntax
infer (Variable x n m) = do
    maybeT <- lookupBind (show $ Variable x n m)
    case maybeT of
        Just t -> return t
        Nothing -> do
            t <- TVar <$> nextTypeVar
            addBind x t
            return t
infer (Tup xs) =
    TCross <$> mapM infer xs
infer (SupTup xs) =
    TCross <$> mapM infer xs

