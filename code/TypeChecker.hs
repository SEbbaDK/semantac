{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# LANGUAGE LambdaCase     #-}

module TypeChecker where

import           Ast
import           Control.Arrow       (left)
import           Control.Monad       (foldM, void)
import           Control.Monad.State (MonadState (get, put), State, evalState)
import           Data.List           as List (intercalate)
import           Data.Map.Strict     as Map (Map, insert, lookup)
import           Data.Maybe          (fromMaybe)
import           Debug.Trace         (trace)
import           Errors
import           Types

{-
What can we check?
- Transitions are consistent with their systems.
- Variables in the conclusion must be defined.
- Variables must not conflict with eachother.
- Syntax nodes must be consistent with the grammar.
-}

type TopResult = Either TopError ()

check :: Top -> TopResult
check (Top domains systems rules) =
    let
        init = Right ()

        f :: TopResult -> Loc Rule -> TopResult
        f (Right ()) rule =
            left (RuleError ruleName) (checkRule domains systems rule)
            where Rule { name = ruleName } = unLoc rule
        f (Left e) rule   = Left e
    in
    foldl f init rules

type RuleResult = Either RuleError ()

checkRule :: [Loc Category] -> [Loc System] -> Loc Rule -> RuleResult
checkRule domains systems (Loc _ Rule {base, premises, properties}) =
    let
        tEnv = newTypeEnv domains systems

        init = Right ()

        f :: RuleResult -> Premise -> State TypeEnv RuleResult
        f (Right ()) prem = do
            tmp <- checkPremiseSystems prem
            return $ left (\case Left e -> toRuleError e ; Right e -> PremiseError e) tmp
        f l _ = return l
    in
    evalState (foldM f init premises) tEnv

type PremiseResult = Either (Either UnifyError PremiseError) ()

-- Premise matches system
checkPremiseSystems :: Premise -> State TypeEnv PremiseResult
checkPremiseSystems (TPremise trans) = do
    let Trans { system } = trans
    sys <- getSystem system
    case sys of
        Nothing  -> return (Left (Right (TransitionError (UndefinedArrow system))))
        Just sys -> fmap
            (left (fmap TransitionError) . void)
            (checkTransSystem sys trans)
checkPremiseSystems (TEquality eq) =
    return (trace "todo: eq" (Right ()))

type TransitionResult = Either (Either UnifyError TransitionError) Type

-- Transition matches system
checkTransSystem :: System -> Trans -> State TypeEnv TransitionResult
checkTransSystem System { arrow, initial, final } Trans { system, before, after } = do
    t1 <- infer before
    applySubst
    t1 <- unify t1 (fromSpec initial)
    case t1 of
        Left e -> return . Left . Left $ e
        Right t1 -> do
            t2 <- infer after
            applySubst
            t2 <- unify t2 (fromSpec final)
            case t2 of
                Left e   -> return . Left . Left $ e
                Right t2 -> do
                    t <- unify t1 t2
                    return $ left Left t

getSystem :: String -> State TypeEnv (Maybe System)
getSystem systemArrow = do
    TypeEnv { systems } <- get
    case filter (\(Loc _ System { arrow }) -> arrow == systemArrow) systems of
        []      -> return Nothing
        sys : _ -> return . Just . unLoc $ sys

newTypeVar :: State TypeEnv TypeVar
newTypeVar = do
    tEnv <- get
    let (tv, nTEnv) = nextTypeVar tEnv
    put nTEnv
    return tv


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
freeTypeVars (TFunc a b)    = freeTypeVars a ++ freeTypeVars b


{- This is essentially just a subset of RuleError, and maybe it should just be replaced by RuleError, but I'm keeping it until we figure out the errors properly because we might need to have slightly different types signatures -}
data UnifyError
  = UInfiniteType TypeVar Type
  | UMismatch Type Type


type UnifyResult = Either UnifyError Type

toRuleError :: UnifyError -> RuleError
toRuleError (UInfiniteType tv t) = VarInifiniteType tv t
toRuleError (UMismatch t1 t2)    = VarTypeMismatch t1 t2

varBind :: TypeVar -> Type -> State TypeEnv UnifyResult
varBind tv t =
    if tv `elem` freeTypeVars t then
        return (Left (UInfiniteType tv t))
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
    -- TODO: Shouldn't there be a unify TUnion?
unify (TFunc a1 b1) (TFunc a2 b2) | a1 == a2 && b1 == b2 =
    -- TODO: Finish the TFunc unification
    error "todo"
unify t1 t2 =
    return (Left (UMismatch t1 t2))

unifyCross :: [(Type, Type)] -> [Type] -> State TypeEnv UnifyResult
unifyCross [] results =
    (return . Right . TCross . reverse) results
unifyCross ((t1_, t2_) : ts_) results = do
    r <- unify t1_ t2_
    case r of
        Right t -> unifyCross ts_ (t : results)
        Left e  -> return (Left e)

infer :: Loc Conf -> State TypeEnv Type
infer (Loc _ (Conf xs)) =
    TCross <$> mapM infer xs
infer (Loc _ (Paren e)) =
    infer e
infer (Loc _ (Syntax _)) =
    return TSyntax
infer (Loc _ (Var (Variable x n m _))) = do
    -- TODO: This should make the inferred variable need to be a
    --       function if there is bindings
    maybeT <- lookupBind (x ++ "_" ++ n ++ replicate m '\'')
    case maybeT of
        Just t -> return t
        Nothing -> do
            t <- TVar <$> newTypeVar
            addBind x t
            return t
infer (Loc _ (SyntaxList xs)) =
    TCross <$> mapM infer xs

