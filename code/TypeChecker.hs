{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module TypeChecker where

import           Ast
import           Control.Arrow       (left)
import           Control.Monad       (foldM, unless, void)
import           Control.Monad.Cont  (lift)
import           Control.Monad.State (MonadState (get, put), StateT (runStateT),
                                      evalStateT, execStateT, mapStateT)
import           Data.Bifunctor      (Bifunctor (bimap))
import           Data.List           as List (any, find, nub, sort)
import           Data.Map.Strict     as Map (Map, empty, insert, lookup)
import           Debug.Trace         (trace)
import           Errors
import           Loc
import           Types

type TopResult = Either (Error TopError) CheckResult

type CheckResult = Map String (Map String Type)

check :: Top -> TopResult
check (Top declarations domains systems rules) =
    let
        f :: TopResult -> Loc Rule -> TopResult
        f (Right allBinds) rule =
            bimap
                (fmap (RuleError ruleName))
                (\binds -> Map.insert ruleName binds allBinds)
                (checkRule domains systems rule)
            where Rule { name = ruleName } = unLoc rule
        f (Left x) rule = Left x
    in
    foldl f (Right mempty) rules

type TCResult e a = StateT TCState (Either (Error e)) a
type TypeChecker m a = StateT TCState m a

data TCState
  = TypeEnv
    { nextTypeVar_ :: TypeVar
    , subs         :: Substitutions
    , bindings     :: Map String Type
    , domains      :: [Loc Category]
    , systems      :: [Loc System]
    , contextStack :: ContextStack
    }

context :: Monad m => Context -> TypeChecker m a -> TypeChecker m a
context ctx s = do
    pushContext
    res <- s
    popContext
    return res
    where
        pushContext :: Monad m => TypeChecker m ()
        pushContext = do
            cCtx <- get
            let TypeEnv { contextStack } = cCtx
            put cCtx { contextStack = ctx : contextStack }
        popContext :: Monad m => TypeChecker m ()
        popContext = do
            cCtx <- get
            let TypeEnv { contextStack } = cCtx
            case contextStack of
                []               -> return ()
                _ : nextCtxStack -> put cCtx { contextStack = nextCtxStack }

checkRule :: [Loc Category] -> [Loc System] -> Loc Rule -> Either (Error RuleError) (Map String Type)
checkRule domains systems rule =
    let tEnv = TypeEnv (TypeVar 1) mempty mempty domains systems [CRule rule] in
    bindings <$> execStateT (checkRule_ domains systems rule) tEnv

checkRule_ :: [Loc Category] -> [Loc System] -> Loc Rule -> TCResult RuleError ()
checkRule_ domains systems rule = do
    let Rule {base, premises, properties} = unLoc rule
    res <- foldM (\() -> checkPremise . fakeLoc) () premises
    context (CConclusion (fakeLoc base)) (checkConclusion (fakeLoc base))

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
        t <- unify fakePos fakePos t1 t2
        TypeEnv {subs} <- get
        return (subst subs t)

inferExpr :: Expr -> TCResult RuleError Type
inferExpr (EVar v) =
  inferVar v
inferExpr (ECall base params) =
  error "Can't infer calls yet"

checkTrans :: Loc System -> Trans -> TCResult RuleError ()
checkTrans sys Trans { system, before, after } = do
    let Loc l System { arrow, initial, final } = sys
    context (CConf before) $ do
        tl <- infer before
        tr <- substTC $ fromSpec $ unLoc initial
        unless (typeMatches tl tr) $
            returnError $ ConfTypeMismatch (Loc (pos before) tl) sys
    context (CConf after) $ do
        tl <- infer after
        tr <- substTC $ fromSpec $ unLoc final
        unless (typeMatches tl tr) $
            returnError $ ConfTypeMismatch (Loc (pos after) tl) sys
    return ()
    where
        substTC :: Type -> TCResult a Type
        substTC t = do
            TypeEnv { subs } <- get
            case subst subs t of
                TNamed x -> lookupType x
                t        -> return t

infer :: Monad m => Loc Conf -> TypeChecker m Type
infer (Loc _ (Conf xs))       = TCross <$> mapM infer xs
infer (Loc _ (Paren e))       = infer e
infer (Loc _ (Syntax _))      = return tSyntax
infer (Loc _ (Var v))         = inferVar v
infer (Loc _ (SyntaxList xs)) = tSyntax <$ mapM infer xs

inferVar :: Monad m => Variable -> TypeChecker m Type
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

newTypeVar :: Monad m => TypeChecker m TypeVar
newTypeVar = do
    tEnv <- get
    let TypeEnv { nextTypeVar_ = TypeVar tv } = tEnv
    put tEnv { nextTypeVar_ = TypeVar (tv + 1) }
    return (TypeVar tv)

lookupBind :: Monad m => String -> TypeChecker m (Maybe Type)
lookupBind name = do
    TypeEnv { bindings } <- get
    return (Map.lookup name bindings)

typeVars :: Type -> [TypeVar]
typeVars (TNamed name)    = []
typeVars (TCategory name) = []
typeVars (TCross xs)      = concatMap typeVars xs
typeVars (TUnion xs)      = concatMap typeVars xs
typeVars (TVar v)         = [v]
typeVars (TFunc a b)      = typeVars a ++ typeVars b

varBind :: TypeVar -> Type -> TCResult RuleError Type
varBind tv t =
    if tv `elem` typeVars t then
        returnError (InifiniteType tv (fakeLoc t))
    else do
        tEnv <- get
        let TypeEnv { subs } = tEnv
        put tEnv { subs = normalizeSubst $ Map.insert tv t subs }
        return t

mapError :: (Error a -> Error b) -> TCResult a t -> TCResult b t
mapError f = mapStateT (left f)

returnError :: a -> TypeChecker (Either (Error a)) b
returnError err = do
    TypeEnv { contextStack } <- get
    lift (Left (Error (contextStack, err)))

lookupType :: String -> TCResult a Type
lookupType name = do
    TypeEnv { domains } <- get
    case List.find (\c -> name == category (unLoc c)) domains of
        Just c  -> return $ fromSpec $ spec $ unLoc c
        Nothing -> return $ TNamed name

{-
do
    TypeEnv { subs } <- get
    unifyBase (subst subs t1) (subst subs t2)
-}
unify :: Pos -> Pos -> Type -> Type -> TCResult RuleError Type
unify lp rp t1_ t2_ =
    case (t1_, t2_) of
        (TNamed x, t2) -> do
            t1 <- lookupType x
            unify lp rp t1 t2
        (t1, TNamed x) -> do
            t2 <- lookupType x
            unify lp rp t1 t2
        (TCategory x1, TCategory x2) | x1 == x2 ->
            return $ TCategory x1
        (t, TVar tv) -> varBind tv t
        (TVar tv, t) -> varBind tv t
        (TCross [t1], t2) -> unify lp rp t1 t2
        (t1, TCross [t2]) -> unify lp rp t1 t2
        (TCross t1, TCross t2) | length t1 == length t2  ->
            unifyCross lp rp (zip t1 t2) []
        (TFunc a1 b1, TFunc a2 b2) -> do
            a <- unify lp rp a1 a2
            b <- unify lp rp b1 b2
            return (TFunc a b)
        (TUnion [l], rs) -> unify lp rp l rs
        (ls, TUnion [r]) -> unify lp rp ls r
        (TUnion ls, TUnion rs) -> return $ TUnion (nub (List.sort (ls ++ rs)))
        (TUnion xs, t) -> unifyUnion lp rp xs xs t
        (t, TUnion xs) -> unifyUnion lp rp xs xs t
        -- We _could_ technically create a `TUnion` of the two types,
        -- but unify only gets used for constraining the types such as
        -- in an equality.
        -- Hence it returns an error when the types can't be matched.
        (t1, t2) -> returnError $ TypeMismatch (Loc lp t1) (Loc rp t2)

unifyCross :: Pos -> Pos -> [(Type, Type)] -> [Type] -> TCResult RuleError Type
unifyCross _ _ [] results =
    return $ TCross (reverse results)
unifyCross lp rp ((t1_, t2_) : ts_) results = do
    t <- unify lp rp t1_ t2_
    unifyCross lp rp ts_ (t : results)

unifyUnion :: Pos -> Pos -> [Type] -> [Type] -> Type -> TCResult RuleError Type
unifyUnion lp rp fullUnion (l : rest) r = do
    env <- get
    case runStateT (unify lp rp l r) env of
        (Right (t, nextEnv)) -> do
            put nextEnv
            return t
        (Left er) ->
            unifyUnion lp rp fullUnion rest r
unifyUnion l1 l2 fullUnion [] t =
    returnError $ TypeMismatch (Loc l1 (TUnion fullUnion)) (Loc l2 t)

typeMatches :: Type -> Type -> Bool
typeMatches (TNamed t1) (TNamed t2)       = t1 == t2
typeMatches (TCategory t1) (TCategory t2) = t1 == t2
typeMatches (TVar _) _                    = True
typeMatches _ (TVar _)                    = True
typeMatches (TCross [t1]) t2              = typeMatches t1 t2
typeMatches t1 (TCross [t2])              = typeMatches t1 t2
typeMatches (TCross t1) (TCross t2) | length t1 == length t2 =
    all (uncurry typeMatches) (zip t1 t2)
typeMatches ls (TUnion [r])         = typeMatches ls r
typeMatches (TUnion ls) (TUnion rs) = all (\t -> any (typeMatches t) rs) ls
typeMatches t (TUnion rs)           = any (typeMatches t) rs
typeMatches t1 t2                   = False
