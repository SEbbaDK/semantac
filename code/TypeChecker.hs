{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# LANGUAGE LambdaCase     #-}

module TypeChecker where

import           Ast
import           Control.Arrow       (left)
import           Control.Monad       (foldM, foldM_, void)
import           Control.Monad.State (MonadState (get, put), State, evalState,
                                      runState)
import           Data.List           as List (intercalate, nub, sort)
import           Data.Map.Strict     as Map (Map, insert, lookup)
import           Data.Maybe          (fromMaybe)
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
        f (Right ()) rule =
            left (fmap (RuleError ruleName)) (checkRule domains systems rule)
            where Rule { name = ruleName } = unLoc rule
        f (Left e) rule   = Left e
    in
    foldl f init rules

type RuleResult = Either (Error RuleError) ()

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

context :: Context -> State TypeEnv a -> State TypeEnv a
context ctx s = do
    pushContext ctx
    res <- s
    popContext
    return res
    where
        pushContext :: Context -> State TypeEnv ()
        pushContext ctx = do
            cCtx <- get
            let TypeEnv { contextStack } = cCtx
            put cCtx { contextStack = ctx : contextStack }
        popContext :: State TypeEnv ()
        popContext = do
            cCtx <- get
            let TypeEnv { contextStack } = cCtx
            case contextStack of
                []               -> return ()
                _ : nextCtxStack -> put cCtx { contextStack = nextCtxStack }

checkRule :: [Loc Category] -> [Loc System] -> Loc Rule -> RuleResult
checkRule domains systems rule =
    let
        tEnv = newTypeEnv domains systems (CRule rule)

        Rule {base, premises, properties} = unLoc rule
    in
    evalState (checkRule_ domains systems rule) tEnv

checkRule_ :: [Loc Category] -> [Loc System] -> Loc Rule -> State TypeEnv RuleResult
checkRule_ domains systems rule =
    let
        f :: RuleResult -> Premise -> State TypeEnv RuleResult
        f (Right ()) prem = checkPremise (fakeLoc prem)
        f l _             = return l

        Rule {base, premises, properties} = unLoc rule
    in do
        res <- foldM f (Right ()) premises
        case res of
            Left e   -> return (Left e)
            Right () -> context (CConclusion (fakeLoc base)) (checkConclusion (fakeLoc base))

-- Premise matches system
checkPremise :: Loc Premise -> State TypeEnv RuleResult
checkPremise (Loc l (TPremise trans)) =
    context (CPremise (Loc l (TPremise trans)))
    (do
        let Trans { system } = trans
        sys <- getSystem system
        case sys of
            Nothing  -> returnError (UndefinedArrow (Loc l system))
            Just sys -> do
                fmap void (checkTransSystem sys trans)
                return (Right ()))
checkPremise (Loc l (EPremise eq)) =
    fmap void $ checkEquality eq

checkConclusion :: Loc Trans -> State TypeEnv RuleResult
checkConclusion (Loc l trans) = do
    let Trans { system } = trans
    sys <- getSystem system
    case sys of
        Nothing  -> returnError (UndefinedArrow (Loc l system))
        Just sys -> do
            fmap void (checkTransSystem sys trans)
            return (Right ())


type TypeResult = Either (Error RuleError) Type

checkEquality :: Equality -> State TypeEnv TypeResult
checkEquality eq = case eq of
  Eq   l r -> eqCheck l r
  InEq l r -> eqCheck l r
  where eqCheck :: Expr -> Expr -> State TypeEnv TypeResult
        eqCheck l r = do
            left <- inferExpr l
            case left of
              Left e -> return $ Left e
              Right t1 -> do
                right <- inferExpr r
                case right of
                  Left e -> return $ Left e
                  Right t2 -> unify t1 t2

inferExpr :: Expr -> State TypeEnv TypeResult
inferExpr (EVar v) =
  Right <$> inferVar v
inferExpr (ECall base params) = do
  -- b <- inferExpr base
  -- p <- fmap inferExpr params
  -- TODO: This should actually find the func def and check things
  error "Can't infer calls yet"


-- Transition matches system
checkTransSystem :: Loc System -> Trans -> State TypeEnv TypeResult
checkTransSystem sys Trans { system, before, after } = do
    let Loc _ System { arrow, initial, final } = sys
    t1 <- context (CConf before) (infer before)
    applySubst
    t1 <- context (CConf after) (unify t1 (fromSpec initial))
    case t1 of
        Left e -> return . Left $ e
        Right t1 -> do
            t2 <- infer after
            applySubst
            t2 <- unify t2 (fromSpec final)
            case t2 of
                Left e   -> return . Left $ e
                Right t2 -> unify t1 t2

infer :: Loc Conf -> State TypeEnv Type
infer (Loc _ (Conf xs)) =
    TCross <$> mapM infer xs
infer (Loc _ (Paren e)) =
    infer e
infer (Loc _ (Syntax _)) =
    return TSyntax
infer (Loc _ (Var v)) = inferVar v
infer (Loc _ (SyntaxList xs)) =
    TCross <$> mapM infer xs


inferVar :: Variable -> State TypeEnv Type
inferVar (Variable x n m _) = do
    -- TODO: This should make the inferred variable need to be a
    --       function if there is bindings
    maybeT <- lookupBind (x ++ "_" ++ n ++ replicate m '\'')
    case maybeT of
        Just t -> return t
        Nothing -> do
            t <- TVar <$> newTypeVar
            addBind x t
            return t


getSystem :: String -> State TypeEnv (Maybe (Loc System))
getSystem systemArrow = do
    TypeEnv { systems } <- get
    case filter (\(Loc loc System { arrow }) -> arrow == systemArrow) systems of
        []      -> return Nothing
        sys : _ -> return (Just sys)

newTypeVar :: State TypeEnv TypeVar
newTypeVar = do
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


typeVars :: Type -> [TypeVar]
typeVars TInteger       = []
typeVars TIdentifier    = []
typeVars TSyntax        = []
typeVars (TCustom name) = []
typeVars (TCross xs)    = concatMap typeVars xs
typeVars (TUnion xs)    = concatMap typeVars xs
typeVars (TVar v)       = [v]
typeVars (TFunc a b)    = typeVars a ++ typeVars b

varBind :: TypeVar -> Type -> State TypeEnv TypeResult
varBind tv t =
    if tv `elem` typeVars t then
        returnError (InifiniteType tv (fakeLoc t))
    else do
        tEnv <- get
        let TypeEnv { subs } = tEnv
        put tEnv { subs = Map.insert tv t subs }
        return (Right t)


returnError :: a -> State TypeEnv (Either (Error a) b)
returnError err = do
    TypeEnv { contextStack } <- get
    return $ Left $ Error (contextStack, err)


unify :: Type -> Type -> State TypeEnv TypeResult
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
unify (TFunc a1 b1) (TFunc a2 b2) = do
    a <- unify a1 a2
    applySubst
    b <- unify b1 b2
    applySubst
    return $ case (a, b) of
        (Right a, Right b) -> Right (TFunc a b)
        (Left e, _)        -> Left e
        (_, Left e)        -> Left e
unify (TUnion ls) (TUnion rs) =
    let nextUnion = TUnion (nub (List.sort (ls ++ rs))) in
    return (Right nextUnion)
unify (TUnion xs) t =
    unifyUnion xs xs t
unify t (TUnion xs) =
    unifyUnion xs xs t
unify t1 t2 =
    returnError $ TypeMismatch t1 (fakeLoc t2)

unifyCross :: [(Type, Type)] -> [Type] -> State TypeEnv TypeResult
unifyCross [] results =
    (return . Right . TCross . reverse) results
unifyCross ((t1_, t2_) : ts_) results = do
    r <- unify t1_ t2_
    case r of
        Right t -> unifyCross ts_ (t : results)
        Left e  -> return (Left e)

unifyUnion :: [Type] -> [Type] -> Type -> State TypeEnv TypeResult
unifyUnion fullUnion (l : rest) r = do
    env <- get
    case runState (unify l r) env of
        (Right t, nextEnv) -> do
            put nextEnv
            applySubst
            return (Right t)
        (_, _) ->
            unifyUnion fullUnion rest r
unifyUnion fullUnion [] t =
    returnError $ TypeMismatch (TUnion fullUnion) (fakeLoc t)
