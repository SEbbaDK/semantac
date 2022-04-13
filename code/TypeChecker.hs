{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module TypeChecker where

import           Ast
import           Control.Arrow       (left)
import           Control.Monad       (foldM, join, liftM, void, when)
import           Control.Monad.Cont  (lift)
import           Control.Monad.State (MonadState (get, put), StateT (runStateT),
                                      evalState, evalStateT, runState)
import           Data.List           as List (any, find, intercalate, nub, sort)
import           Data.Map.Strict     as Map (Map, insert, lookup)
import           Debug.Trace         (trace)
import           Errors
import           Loc
import           Types

type TopResult = Either (Error TopError) ()

check :: Top -> TopResult
check (Top declarations domains systems rules) =
    let
        init = Right ()

        f :: TopResult -> Loc Rule -> TopResult
        f (Right x) rule =
            left (fmap (RuleError ruleName)) (checkRule domains systems rule)
            where Rule { name = ruleName } = unLoc rule
        f (Left x) rule = Left x
    in
    foldl f init rules

type TCResult e a = StateT TypeEnv (Either (Error e)) a
type TCState m a = StateT TypeEnv m a

data TypeEnv
  = TypeEnv
    { nextTypeVar_ :: TypeVar
    , subs         :: Substitutions
    , bindings     :: Map String Type
    , domains      :: [Loc Category]
    , systems      :: [Loc System]
    , contextStack :: ContextStack
    }
newTypeEnv :: [Loc Category] -> [Loc System] -> Context -> TypeEnv
newTypeEnv domains systems context = TypeEnv
    { nextTypeVar_ = TypeVar 1
    , subs = mempty
    , bindings = mempty
    , domains
    , systems
    , contextStack = [context]
    }

context :: Monad m => Context -> TCState m a -> TCState m a
context ctx s = do
    pushContext
    res <- s
    popContext
    return res
    where
        pushContext :: Monad m => TCState m ()
        pushContext = do
            cCtx <- get
            let TypeEnv { contextStack } = cCtx
            put cCtx { contextStack = ctx : contextStack }
        popContext :: Monad m => TCState m ()
        popContext = do
            cCtx <- get
            let TypeEnv { contextStack } = cCtx
            case contextStack of
                []               -> return ()
                _ : nextCtxStack -> put cCtx { contextStack = nextCtxStack }

checkRule :: [Loc Category] -> [Loc System] -> Loc Rule -> Either (Error RuleError) ()
checkRule domains systems rule =
    let tEnv = newTypeEnv domains systems (CRule rule) in
    evalStateT (checkRule_ domains systems rule) tEnv

checkRule_ :: [Loc Category] -> [Loc System] -> Loc Rule -> TCResult RuleError ()
checkRule_ domains systems rule = do
    let Rule {base, premises, properties} = unLoc rule
    res <- foldM (\() -> checkPremise . fakeLoc) () premises
    context (CConclusion (fakeLoc base)) (checkConclusion (fakeLoc base))

-- Premise matches system
checkPremise :: Loc Premise -> TCResult RuleError ()
checkPremise (Loc l (TPremise trans)) =
    context (CPremise (Loc l (TPremise trans))) $
    do
        let Trans { system } = trans
        sys <- getSystem system (UndefinedArrow (Loc l system))
        void (checkTrans sys trans)
checkPremise (Loc l (EPremise eq)) =
    void $ checkEquality eq

checkConclusion :: Loc Trans -> TCResult RuleError ()
checkConclusion (Loc l trans) = do
    let Trans { system } = trans
    sys <- getSystem system (UndefinedArrow (Loc l system))
    checkTrans sys trans



checkEquality :: Equality -> TCResult RuleError Type
checkEquality eq = case eq of
  Eq   l r -> eqCheck l r
  InEq l r -> eqCheck l r
  where
    eqCheck :: Expr -> Expr -> TCResult RuleError Type
    eqCheck l r = do
        t1 <- inferExpr l
        t2 <- inferExpr r
        unify t1 t2

inferExpr :: Expr -> TCResult RuleError Type
inferExpr (EVar v) =
  inferVar v
inferExpr (ECall base params) = do
  -- b <- inferExpr base
  -- p <- fmap inferExpr params
  -- TODO: This should actually find the func def and check things
  error "Can't infer calls yet"


-- Transition matches system
checkTrans :: Loc System -> Trans -> TCResult RuleError ()
checkTrans sys Trans { system, before, after } = do
    let Loc _ System { arrow, initial, final } = sys
    t1 <- context (CConf before) $ do
        t <- infer before
        unify t (fromSpec initial)
    t2 <- context (CConf after) $ do
        t <- infer after
        unify t (fromSpec final)
    return ()

infer :: Monad m => Loc Conf -> TCState m Type
infer (Loc _ (Conf xs))       = TCross <$> mapM infer xs
infer (Loc _ (Paren e))       = infer e
infer (Loc _ (Syntax _))      = return tSyntax
infer (Loc _ (Var v))         = inferVar v
infer (Loc _ (SyntaxList xs)) = tSyntax <$ mapM infer xs



inferVar :: Monad m => Variable -> TCState m Type
inferVar (Variable x n m _) = do
    -- TODO: This should make the inferred variable need to be a
    --       function if there is bindings
    maybeT <- lookupBind (x ++ "_" ++ n ++ replicate m '\'')
    case maybeT of
        Just t -> return t
        Nothing -> do
            t <- TVar <$> newTypeVar
            tEnv <- get
            let TypeEnv { bindings } = tEnv
            put tEnv { bindings = Map.insert x t bindings }
            return t

getSystem :: String -> RuleError -> TCResult RuleError (Loc System)
getSystem systemArrow err = do
    TypeEnv { systems } <- get
    case filter (\(Loc loc System { arrow }) -> arrow == systemArrow) systems of
        []      -> returnError err
        sys : _ -> return sys

newTypeVar :: Monad m => TCState m TypeVar
newTypeVar = do
    tEnv <- get
    let TypeEnv { nextTypeVar_ = TypeVar tv } = tEnv
    put tEnv { nextTypeVar_ = TypeVar (tv + 1) }
    return (TypeVar tv)


lookupBind :: Monad m => String -> TCState m (Maybe Type)
lookupBind name = do
    TypeEnv { bindings } <- get
    return (Map.lookup name bindings)


{-
Normalizes the substitutions. All values of the substitution are mapped using `subst` recursively until a fixpoint is reached.

If a Type contains the TypeVar it is keyed on, then this will not terminate. We ensure this does not happen in the `varBind` function.
-}
normalizeSubstTc :: TCResult e ()
normalizeSubstTc = do
    tEnv <- get
    let TypeEnv { subs } = tEnv
    put tEnv { subs = normalizeSubst subs }


typeVars :: Type -> [TypeVar]
typeVars (TNamed name) = []
typeVars (TCross xs)   = concatMap typeVars xs
typeVars (TUnion xs)   = concatMap typeVars xs
typeVars (TVar v)      = [v]
typeVars (TFunc a b)   = typeVars a ++ typeVars b

varBind :: TypeVar -> Type -> TCResult RuleError Type
varBind tv t =
    if tv `elem` typeVars t then
        returnError (InifiniteType tv (fakeLoc t))
    else do
        tEnv <- get
        let TypeEnv { subs } = tEnv
        put tEnv { subs = Map.insert tv t subs }
        return t


returnError :: a -> TCState (Either (Error a)) b
returnError err = do
    TypeEnv { contextStack } <- get
    lift (Left (Error (contextStack, err)))

lookupType :: String -> TCResult RuleError Type
lookupType name = do
    TypeEnv { domains } <- get
    case List.find (\c -> name == category (unLoc c)) domains of
        Just c  -> return $ fromSpec $ spec $ unLoc c
        Nothing -> return $ TNamed name


unify :: Type -> Type -> TCResult RuleError Type
unify t1 t2 = do
    TypeEnv { subs } <- get
    t <- unifyBase (subst subs t1) (subst subs t2)
    normalizeSubstTc
    return t
    where
        unifyBase :: Type -> Type -> TCResult RuleError Type
        unifyBase (TNamed x1) (TNamed x2)  =
            if x1 == x2 then
                return (TNamed x1)
            else do
                t1 <- lookupType x1
                t2 <- lookupType x2
                if t1 == t2 then
                    return t1
                else
                    returnError $ TypeMismatch t1 (fakeLoc t2)
        unifyBase t (TVar tv) =
            varBind tv t
        unifyBase (TVar tv) t =
            varBind tv t
        unifyBase (TCross [t1]) t2 = unifyBase t1 t2
        unifyBase t1 (TCross [t2]) = unifyBase t1 t2
        unifyBase (TCross t1) (TCross t2) | length t1 == length t2  =
            trace (show (TCross t1) ++ show (TCross t2)) $
            unifyCross (zip t1 t2) []
        unifyBase (TFunc a1 b1) (TFunc a2 b2) = do
            a <- unify a1 a2
            b <- unify b1 b2
            return (TFunc a b)
        unifyBase (TUnion [l]) rs = unifyBase l rs
        unifyBase ls (TUnion [r]) = unifyBase ls r
        unifyBase (TUnion ls) (TUnion rs) =
            let nextUnion = TUnion (nub (List.sort (ls ++ rs))) in
            return nextUnion
        unifyBase (TUnion xs) t =
            unifyUnion xs xs t
        unifyBase t (TUnion xs) =
            unifyUnion xs xs t
        unifyBase t1 t2 =
            returnError $ TypeMismatch t1 (fakeLoc t2)

        unifyCross :: [(Type, Type)] -> [Type] -> TCResult RuleError Type
        unifyCross [] results =
            (return . TCross . reverse) results
        unifyCross ((t1_, t2_) : ts_) results = do
            t <- unify t1_ t2_
            unifyCross ts_ (t : results)

        unifyUnion :: [Type] -> [Type] -> Type -> TCResult RuleError Type
        unifyUnion fullUnion (l : rest) r = do
            env <- get
            case runStateT (unify l r) env of
                (Right (t, nextEnv)) -> do
                    put nextEnv
                    normalizeSubstTc
                    return t
                (Left er) ->
                    unifyUnion fullUnion rest r
        unifyUnion fullUnion [] t =
            returnError $ TypeMismatch (TUnion fullUnion) (fakeLoc t)
