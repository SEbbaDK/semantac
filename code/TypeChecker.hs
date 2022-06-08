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
import           Debug.Trace
import           Errors
import           Loc
import           Types

data TCState
  = TCState
    { nextTypeVar_ :: Int
    , bindings     :: Map Variable TypeVar
    , subs         :: Map TypeVar Type
    , terms        :: [Loc TermDecl]
    , categories   :: [Loc CategoryDecl]
    , systems      :: [Loc SystemDecl]
    , contextStack :: ContextStack
    }

type TypeChecker m a = StateT TCState m a
type TCResult e a = TypeChecker (Either (Error e)) a


-- Type Checking

type TypeMap = Map Variable Type
type CheckResult = (Rule, TypeMap)
type SpecificationResult = ([Error SpecificationError], [CheckResult])

constructCheckerState specification context =
    TCState
        { nextTypeVar_ = 1
        , bindings = mempty
        , subs = mempty
        , terms = sTerms specification
        , categories = sCategories specification
        , systems = sSystems specification
        , contextStack = context }

typeCheck :: Specification -> Either [Error SpecificationError] [CheckResult]
typeCheck spec =
    case foldl f ([], []) (sRules spec) of
        ([], oks) -> Right oks
        (errs, _) -> Left errs
    where
        f :: SpecificationResult -> Loc Rule -> SpecificationResult
        f (errors, oks) rule =
            case checkRule spec rule of
                Left err -> (fmap (RuleError rule) err : errors, oks)
                Right ok -> (errors, (unLoc rule, ok) : oks)

checkRule ::
    Specification
    -> Loc Rule
    -> Either (Error RuleError) (Map Variable Type)
checkRule spec rule = do
    let state = constructCheckerState spec [CRule rule]
    TCState {bindings, subs} <- execStateT (checkRuleHelper rule) state
    return $ fmap (subst subs . TVar) bindings

checkRuleHelper :: Loc Rule -> TCResult RuleError ()
checkRuleHelper rule = do
    let Rule {base, premises} = unLoc rule
    foldM_ (\() -> checkPremise) () premises
    context (CConclusion base) (checkTransition base)

checkPremise :: Loc Premise -> TCResult RuleError ()
checkPremise (Loc l (PTransition trans)) =
    context (CPremise (Loc l (PTransition trans))) $ checkTransition (Loc l trans)
checkPremise (Loc l (PConstraint ex)) = do
    ext <- inferExpr l ex
    unify l l ext tBool
    return ()
checkPremise (Loc loc (PDefinition l r)) =
    checkEquality loc loc l r

checkTransition :: Loc Transition -> TCResult RuleError ()
checkTransition (Loc l trans) = do
    sys <- lookupSystem l (system trans)
    checkTransitionHelper sys trans

checkTransitionHelper :: Loc SystemDecl -> Transition -> TCResult RuleError ()
checkTransitionHelper sys Transition { before, after } = do
    let Loc l SystemDecl { initial, final } = sys
    context (CConf before) $ do
        tl <- inferConf before
        let tr = unLoc initial
        mapError (mismatch sys initial) $ unify (pos before) (pos initial) tl tr
    context (CConf after) $ do
        tl <- inferConf after
        let tr = unLoc final
        mapError (mismatch sys final) $ unify (pos after) (pos final) tl tr
    return ()
    where
        mismatch :: Loc SystemDecl -> Loc Type -> RuleError -> RuleError
        mismatch sys sysdef (TypeMismatch use def) =
            ConfTypeMismatch use def $ Loc (pos sysdef) $ unLoc sys
        mismatch _ _ e = e

checkEquality :: Pos -> Pos -> Expr -> Expr -> TCResult RuleError ()
checkEquality lp rp l r = do
    t1 <- inferExpr lp l
    t2 <- inferExpr rp r
    unify lp rp t1 t2
    return ()

-- Inference

inferConf :: Loc Conf -> TCResult RuleError Type
inferConf (Loc _ (Conf xs)) = TCross <$> mapM inferConfElement xs
    where inferConfElement [e] = inferSyntax e
          inferConfElement lis = return tSyntax

inferSyntax :: Loc SyntaxElem -> TCResult RuleError Type
inferSyntax (Loc p syntax) = case syntax of
    SubElem e -> TCross <$> mapM inferSyntax e
    Syntax _  -> return tSyntax
    Var v     -> inferVarEx v

inferVarEx :: VariableExpr -> TCResult RuleError Type
inferVarEx (VRef v) = inferVar v
inferVarEx (VBind v l r) = do
    vt <- inferVarEx v
    lt <- inferVar l
    rt <- inferExpr fakePos r
    unify fakePos fakePos vt (TFunc lt rt)

-- TODO: This should make the inferred variable need to be a
--       function if there is bindings
inferVar :: Variable -> TCResult RuleError Type
inferVar Variable { typeName = Just tName }
    = lookupType fakePos tName
inferVar x
    = TVar <$> typeVarOf x

inferExpr :: Pos -> Expr -> TCResult RuleError Type
inferExpr pos (EVar v) = inferVarEx v
inferExpr pos (EEq l r) = do
    lt <- inferExpr pos l
    rt <- inferExpr pos r
    unify pos pos lt rt
inferExpr pos (EInEq l r) = do
    lt <- inferExpr pos l
    rt <- inferExpr pos r
    unify pos pos lt rt
inferExpr pos (ECall f args) = do
    tf <- inferExpr pos f
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
        (TPrimitive x1, TPrimitive x2) | x1 == x2 ->
            return $ TPrimitive x1
        (TCross lts, TCross rts) | length lts == length rts ->
            TCross <$> zipWithM (unify lp rp) lts rts
        (TFunc lat lrt, TFunc rat rrt) -> do
            a <- unify lp rp lat rat
            b <- unify lp rp lrt rrt
            return (TFunc a b)
        (TAlias x, _)    -> lookupType lp x >>= \lt -> unify lp rp lt rt
        (TVar tv, _)     -> bindVar tv rt
        (TCross [lt], _) -> unify lp rp lt rt
        (TUnion [lt], _) -> unify lp rp lt rt
        (TUnion lts, _)  -> unifyUnion lp rp lts lts rt
        (_, TAlias x)    -> swap $ unify rp lp (TAlias x) lt
        (_, TVar tv)     -> swap $ unify rp lp (TVar tv) lt
        (_, TCross rts)  -> swap $ unify rp lp (TCross rts) lt
        (_, TUnion rts)  -> swap $ unify rp lp (TUnion rts) lt
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

lookupSystem :: Pos -> String -> TCResult RuleError (Loc SystemDecl)
lookupSystem pos systemArrow = do
    TCState { systems } <- get
    case find ((systemArrow ==) . arrow . unLoc) systems of
        Just sys -> return sys
        Nothing  -> returnError (UndefinedArrow (Loc pos systemArrow))

lookupTerm :: Pos -> String -> TCResult RuleError Type
lookupTerm pos name = do
    TCState { terms } <- get
    case termLookup name terms of
        Just dec -> return $ dType (unLoc dec)
        Nothing  -> returnError (UndefinedTerm (Loc pos name))

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
            newstate <- get
            put newstate {bindings = insert x t (bindings state)}
            return t

bindVar :: TypeVar -> Type -> TCResult RuleError Type
bindVar tv t =
    if tv `elem` typeVars t then
        returnError (InfiniteType tv (fakeLoc t))
    else do
        state <- get
        let TCState { subs } = state
        put state { subs = normalizeSubst $ insert tv t subs }
        return t

-- substTC :: Monad m => Type -> TypeChecker m Type
-- substTC t = do
--     TCState { subs } <- get
--     subst subs t

-- `substTC` performs a substitution using the current substitution map.
-- The `subst` function traverses the given type and replaces type variables with the types that they map to in the substitution map if they are defined there.

returnError :: a -> TypeChecker (Either (Error a)) b
returnError err = do
    TCState { contextStack } <- get
    lift (Left (Error (contextStack, err)))

lookupType :: Pos -> String -> TCResult RuleError Type
lookupType p name = do
    TCState { categories } <- get
    case categoryLookup name categories of
        Just c  -> return $ cType $ unLoc c
        Nothing -> returnError $ UndefinedType (Loc p name)

swap = mapError m
    where
        m (TypeMismatch a b) = TypeMismatch b a
        m e = e

mapError :: (RuleError -> RuleError)
         -> TCResult RuleError t
         -> TCResult RuleError t
mapError f = mapStateT (left . fmap $ f)
