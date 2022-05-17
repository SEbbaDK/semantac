{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module TypeChecker where

import           Ast
import           Control.Arrow       (left)
import           Control.Monad       (foldM_, zipWithM)
import           Control.Monad.Cont  (lift)
import           Control.Monad.State (MonadState (get, put), StateT (runStateT),
                                      execStateT, mapStateT, modify)
import           Data.Bifunctor      (Bifunctor (bimap))
import           Data.List           (find)
import           Data.Map.Strict     as Map (Map, insert, lookup)
import           Errors
import           Loc
import           Types
import Debug.Trace

data TCState
  = TCState
    { nextTypeVar_ :: Int
    , bindings     :: Map Variable TypeVar
    , subs         :: Substitutions
    , declarations :: [Loc Declaration]
    , categories   :: [Loc Category]
    , systems      :: [Loc System]
    , contextStack :: ContextStack
    }

type TypeChecker m a = StateT TCState m a
type TCResult e a = TypeChecker (Either (Error e)) a


-- Type Checking

type CheckResult = Map String (Map Variable Type)
type SpecificationResult = Either (Error SpecificationError) CheckResult

check :: Specification -> SpecificationResult
check (Specification declarations categories systems rules) =
    foldl f (Right mempty) rules
    where
        f :: SpecificationResult -> Loc Rule -> SpecificationResult
        f (Right allBinds) rule =
            bimap
                (fmap (RuleError ruleName))
                (\binds -> insert ruleName binds allBinds)
                (checkRule declarations categories systems rule)
            where Rule { name = ruleName } = unLoc rule
        f (Left x) rule = Left x

checkRule :: [Loc Declaration] -> [Loc Category] -> [Loc System] -> Loc Rule -> Either (Error RuleError) (Map Variable Type)
checkRule declarations categories systems rule = do
    let state = TCState 1 mempty mempty declarations categories systems [CRule rule]
    TCState {bindings, subs} <- execStateT (checkRuleHelper rule) state
    return $ fmap (subst subs . TVar) bindings


checkRuleHelper :: Loc Rule -> TCResult RuleError ()
checkRuleHelper rule = do
    let Rule {base, premises, properties} = unLoc rule
    foldM_ (\() -> checkPremise . fakeLoc) () premises
    context (CConclusion (fakeLoc base)) (checkTransition (fakeLoc base))

checkPremise :: Loc Premise -> TCResult RuleError ()
checkPremise (Loc l (PTransition trans)) =
    context (CPremise (Loc l (PTransition trans))) $ checkTransition (Loc l  trans)
checkPremise (Loc l (PEquality eq)) =
    checkEquality l eq

checkTransition :: Loc Transition -> TCResult RuleError ()
checkTransition (Loc l trans) = do
    sys <- lookupSystem l (system trans)
    checkTransitionHelper sys trans

checkTransitionHelper :: Loc System -> Transition -> TCResult RuleError ()
checkTransitionHelper sys Transition { before, after } = do
    let Loc l System { initial, final } = sys
    context (CConf before) $ do
        tl <- infer before
        let tr = unLoc initial
        mapError (mismatch sys initial) $ unify (pos before) (pos initial) tl tr
    context (CConf after) $ do
        tl <- infer after
        let tr = unLoc final
        mapError (mismatch sys final) $ unify (pos after) (pos final) tl tr
    return ()
    where
        mismatch :: Loc System -> Loc Type -> Error RuleError -> Error RuleError
        mismatch sys sysdef (Error (s, TypeMismatch use def)) =
            Error (s, ConfTypeMismatch use def $ Loc (pos sysdef) $ unLoc sys)
        mismatch _ _ e = e

checkEquality :: Pos -> Equality -> TCResult RuleError ()
checkEquality pos (Eq l r)   = checkEqualityHelper pos pos l r
checkEquality pos (InEq l r) = checkEqualityHelper pos pos l r

checkEqualityHelper :: Pos -> Pos -> Expr -> Expr -> TCResult RuleError ()
checkEqualityHelper lp rp l r = do
    t1 <- inferExpr lp l
    t2 <- inferExpr rp r
    unify lp rp t1 t2
    return ()

-- Inference

infer :: Monad m => Loc Conf -> TypeChecker m Type
infer (Loc _ (Conf xs))       = TCross <$> mapM infer xs
infer (Loc _ (Paren e))       = infer e
infer (Loc _ (Syntax _))      = return tSyntax
infer (Loc _ (Var v))         = inferVar v
-- sure, we can _discover_ variables by traversing the SyntaxList, but since we don't have the grammar of the language
-- we can't actually generate any constraints from this. We'll just have variables bound to generic type variables.
-- So maybe we shouldn't bother? If a configuration can be an expression, i.e. a function application, then we 
-- could use those to generate constraints.
-- TODO: decide if we're adding expressions to the configuration or not.
infer (Loc _ (SyntaxList xs)) = tSyntax <$ mapM infer xs

-- TODO: This should make the inferred variable need to be a
--       function if there is bindings
inferVar :: Monad m => Variable -> TypeChecker m Type
inferVar x = TVar <$> typeVarOf x


inferExpr :: Pos -> Expr -> TCResult RuleError Type
inferExpr pos (EVar v)            = inferVar v
inferExpr pos (ECall f args) = do
    tf <- lookupFunction pos f
    ta <- TCross <$> mapM (inferExpr pos) args
    tr <- TVar <$> newTypeVar
    tf <- unify pos pos tf (TFunc ta tr) 
    TCState { subs } <- get
    return $ subst subs tr


-- Unification

-- Note: It seems we never use the results of unifying, only the constraints... We could change the return type to ().
unify :: Pos -> Pos -> Type -> Type -> TCResult RuleError Type
unify lp rp lt rt =
    case (lt, rt) of
        (TCategory x1, TCategory x2) | x1 == x2 ->
            return $ TCategory x1
        (TCross lts, TCross rts) | length lts == length rts ->
            TCross <$> zipWithM (unify lp rp) lts rts
        (TFunc lat lrt, TFunc rat rrt) -> do
            a <- unify lp rp lat rat
            b <- unify lp rp lrt rrt
            return (TFunc a b)
        (TNamed x, _)    -> lookupType lp x >>= \lt -> unify lp rp lt rt
        (TVar tv, _)     -> bindVar tv rt
        (TCross [lt], _) -> unify lp rp lt rt
        (TUnion [lt], _) -> unify lp rp lt rt
        (TUnion lts, _)  -> unifyUnion lp rp lts lts rt
        (_, TNamed x)    -> unify rp lp (TNamed x) lt
        (_, TVar tv)     -> unify rp lp (TVar tv) lt
        (_, TCross rts)  -> unify rp lp (TCross rts) lt
        (_, TUnion rts)  -> unify rp lp (TUnion rts) lt
        -- We _could_ technically create a `TUnion` of the two types,
        -- but unify only gets used for constraining the types such as
        -- in an equality.
        -- Hence it returns an error when the types can't be matched.
        _               -> returnError $ TypeMismatch (Loc lp lt) (Loc rp rt)

unifyUnion :: Pos -> Pos -> [Type] -> [Type] -> Type -> TCResult RuleError Type
unifyUnion lp rp fullUnion [] t =
    returnError $ TypeMismatch (Loc lp (TUnion fullUnion)) (Loc rp t)
unifyUnion lp rp fullUnion (l : rest) r = do
    state <- get
    case runStateT (unify lp rp l r) state of
        Left _  -> unifyUnion lp rp fullUnion rest r
        Right (x, nextState) -> do
            put nextState
            return x


-- Helper functions

context :: Monad m => Context -> TypeChecker m a -> TypeChecker m a
context ctx s = do
    pushContext
    res <- s
    popContext
    return res
    where
        pushContext = modify (\cCtx -> cCtx { contextStack = ctx : contextStack cCtx })
        popContext  = modify (\cCtx -> cCtx { contextStack = drop 1 (contextStack cCtx) })

lookupSystem :: Pos -> String -> TCResult RuleError (Loc System)
lookupSystem pos systemArrow = do
    TCState { systems } <- get
    case find ((systemArrow ==) . arrow . unLoc) systems of
        Just sys -> return sys
        Nothing  -> returnError (UndefinedArrow (Loc pos systemArrow))

lookupFunction :: Pos -> String -> TCResult RuleError Type
lookupFunction pos name = do
    TCState { declarations } <- get
    case find ((name ==) . fName . unLoc) declarations of
        Just dec -> return $ fType (unLoc dec)
        Nothing  -> returnError (UndefinedVar (Loc pos name))

newTypeVar :: Monad m => TypeChecker m TypeVar
newTypeVar = do
    state <- get
    let tv = nextTypeVar_ state
    put state { nextTypeVar_ = tv + 1 }
    return (TypeVar tv)

typeVarOf :: Monad m => Variable -> TypeChecker m TypeVar
typeVarOf x = do
    state <- get
    case Map.lookup x (bindings state) of
        Just t -> return t
        Nothing -> do
            t <- newTypeVar
            put state {bindings = insert x t (bindings state)}
            return t

bindVar :: TypeVar -> Type -> TCResult RuleError Type
bindVar tv t =
    if tv `elem` typeVars t then
        returnError (InifiniteType tv (fakeLoc t))
    else do
        state <- get
        let TCState { subs } = state
        put state { subs = normalizeSubst $ insert tv t subs }
        return t

-- substTC :: Monad m => Type -> TypeChecker m Type
-- substTC t = do
--     TCState { subs } <- get
--     subst subs t

-- \texttt{substTC} performs a substitution using the current substitution map. 
-- The \texttt{subst} function traverses the given type and replaces type variables with the types that they map to in the substitution map if they are defined there.

returnError :: a -> TypeChecker (Either (Error a)) b
returnError err = do
    TCState { contextStack } <- get
    lift (Left (Error (contextStack, err)))

lookupType :: Pos -> String -> TCResult RuleError Type
lookupType p name = do
    TCState { categories } <- get
    case find ((name ==) . cName . unLoc) categories of
        Just c  -> return $ cType $ unLoc c
        Nothing -> returnError $ UndefinedType (Loc p name)

mapError :: (Error a -> Error b) -> TCResult a t -> TCResult b t
mapError f = mapStateT (left f)
