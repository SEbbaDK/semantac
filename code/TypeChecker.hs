{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module TypeChecker where

import           Ast
import           Control.Arrow       (left)
import           Control.Monad       (foldM_)
import           Control.Monad.Cont  (lift)
import           Control.Monad.State (MonadState (get, put), StateT (runStateT),
                                      execStateT, mapStateT)
import           Data.Bifunctor      (Bifunctor (bimap))
import           Data.List           (find, nub, sort)
import           Data.Map.Strict     as Map (Map, insert, lookup)
import           Errors
import           Loc
import           Types

data TCState
  = TypeEnv
    { nextTypeVar_ :: TypeVar
    , subs         :: Substitutions
    , bindings     :: Map String Type
    , domains      :: [Loc Category]
    , systems      :: [Loc System]
    , contextStack :: ContextStack
    }

type TypeChecker m a = StateT TCState m a
type TCResult e a = TypeChecker (Either (Error e)) a


-- Type Checking

type CheckResult = Map String (Map String Type)
type SpecificationResult = Either (Error SpecificationError) CheckResult

check :: Specification -> SpecificationResult
check (Specification declarations domains systems rules) =
    foldl f (Right mempty) rules
    where
        f :: SpecificationResult -> Loc Rule -> SpecificationResult
        f (Right allBinds) rule =
            bimap
                (fmap (RuleError ruleName))
                (\binds -> insert ruleName binds allBinds)
                (checkRule domains systems rule)
            where Rule { name = ruleName } = unLoc rule
        f (Left x) rule = Left x

checkRule :: [Loc Category] -> [Loc System] -> Loc Rule -> Either (Error RuleError) (Map String Type)
checkRule domains systems rule =
    let tEnv = TypeEnv (TypeVar 1) mempty mempty domains systems [CRule rule] in
    bindings <$> execStateT (checkRule_ rule) tEnv

checkRule_ :: Loc Rule -> TCResult RuleError ()
checkRule_ rule = do
    let Rule {base, premises, properties} = unLoc rule
    foldM_ (\() -> checkPremise . fakeLoc) () premises
    context (CConclusion (fakeLoc base)) (checkConclusion (fakeLoc base))

checkPremise :: Loc Premise -> TCResult RuleError ()
checkPremise (Loc l (PTransition trans)) =
    context (CPremise (Loc l (PTransition trans))) $
    do
        let Transition { system } = trans
        sys <- lookupSystem l system
        checkTrans sys trans
checkPremise (Loc l (PEquality eq)) =
    checkEquality eq

checkConclusion :: Loc Transition -> TCResult RuleError ()
checkConclusion (Loc l trans) = do
    let Transition { system } = trans
    sys <- lookupSystem l system
    checkTrans sys trans

checkEquality :: Equality -> TCResult RuleError ()
checkEquality eq = case eq of
  Eq   l r -> eqCheck l r
  InEq l r -> eqCheck l r
  where
    eqCheck :: Expr -> Expr -> TCResult RuleError ()
    eqCheck l r = do
        t1 <- inferExpr l
        t2 <- inferExpr r
        unify fakePos fakePos t1 t2
        return ()

checkTrans :: Loc System -> Transition -> TCResult RuleError ()
checkTrans sys Transition {  before, after } = do
    let Loc l System { arrow, initial, final } = sys
    context (CConf before) $ do
        tl <- infer before
        tr <- substTC $ fromSpec $ unLoc initial
        unify (pos before) (pos initial) tl tr
    context (CConf after) $ do
        tl <- infer after
        tr <- substTC $ fromSpec $ unLoc final
        unify (pos after) (pos final) tl tr
    return ()
    where
        substTC :: Type -> TCResult a Type
        substTC t = do
            TypeEnv { subs } <- get
            case subst subs t of
                TNamed x -> lookupType x
                t        -> return t


-- Inference

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
            put tEnv { bindings = insert x t bindings }
            return t

inferExpr :: Expr -> TCResult RuleError Type
inferExpr (EVar v) =
  inferVar v
inferExpr (ECall base params) =
  error "Can't infer calls yet"


-- Unification

-- Note: It seems we never use the results of unifying, only the constraints... We could change the return type to ().
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
        (TUnion ls, TUnion rs) -> return $ TUnion (nub (sort (ls ++ rs)))
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


-- Helper functions

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

lookupSystem :: Pos -> String -> TCResult RuleError (Loc System)
lookupSystem pos systemArrow = do
    TypeEnv { systems } <- get
    case filter (\(Loc loc System { arrow }) -> arrow == systemArrow) systems of
        []      -> returnError (UndefinedArrow (Loc pos systemArrow))
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

varBind :: TypeVar -> Type -> TCResult RuleError Type
varBind tv t =
    if tv `elem` typeVars t then
        returnError (InifiniteType tv (fakeLoc t))
    else do
        tEnv <- get
        let TypeEnv { subs } = tEnv
        put tEnv { subs = normalizeSubst $ insert tv t subs }
        return t

returnError :: a -> TypeChecker (Either (Error a)) b
returnError err = do
    TypeEnv { contextStack } <- get
    lift (Left (Error (contextStack, err)))

lookupType :: String -> TCResult a Type
lookupType name = do
    TypeEnv { domains } <- get
    case find (\c -> name == cName (unLoc c)) domains of
        Just c  -> return $ fromSpec $ spec $ unLoc c
        Nothing -> return $ TNamed name

mapError :: (Error a -> Error b) -> TCResult a t -> TCResult b t
mapError f = mapStateT (left f)
