module Types where

import           Control.Monad       (foldM, void)
import           Control.Monad.State (MonadState (get, put), State, evalState)
import           Data.List           as List (intercalate)
import           Data.Map.Strict     as Map (Map, insert, lookup)
import           Data.Maybe          (fromMaybe)
import           Debug.Trace         (trace)
import           Loc

data Type
  = TAlias String
  | TPrimitive String
  | TCross [Type]
  | TUnion [Type]
  | TFunc Type Type
  | TVar TypeVar
  deriving (Eq, Ord, Show)

tSyntax :: Type
tSyntax = TPrimitive "Syntax"

tBool :: Type
tBool = TPrimitive "Bool"

newtype TypeVar
  = TypeVar Int
  deriving (Eq, Ord, Show)

typeVars :: Type -> [TypeVar]
typeVars (TAlias name)     = []
typeVars (TPrimitive name) = []
typeVars (TCross xs)       = concatMap typeVars xs
typeVars (TUnion xs)       = concatMap typeVars xs
typeVars (TVar v)          = [v]
typeVars (TFunc a b)       = typeVars a ++ typeVars b

type Substitutions = Map TypeVar Type

subst :: Substitutions -> Type -> Type
subst subs (TAlias x)     = TAlias x
subst subs (TPrimitive x) = TPrimitive x
subst subs (TCross xs)    = TCross (fmap (subst subs) xs)
subst subs (TUnion xs)    = TUnion (fmap (subst subs) xs)
subst subs (TFunc a b)    = TFunc (subst subs a) (subst subs b)
subst subs (TVar tv)      = fromMaybe (TVar tv) (Map.lookup tv subs)


{-
Normalizes the substitutions. All values of the substitution are mapped using `subst` recursively until a fixpoint is reached.

If a Type contains the TypeVar it is keyed on, then this will not terminate. We ensure this does not happen in the `varBind` function.
-}
normalizeSubst :: Substitutions -> Substitutions
normalizeSubst subs =
    let nextSubs = fmap (subst subs) subs in
    if nextSubs /= subs then
        normalizeSubst nextSubs
    else
        subs


normalizeType :: Type -> Type
normalizeType (TCross ts) = TCross $ normalizeCross ts
normalizeType (TUnion ts) = TUnion $ normalizeUnion ts
normalizeType (TFunc t1 t2) = TFunc (normalizeType t1) (normalizeType t2)
normalizeType t = t

normalizeCross :: [Type] -> [Type]
normalizeCross (TCross ts : rest) = normalizeCross (ts ++ rest)
normalizeCross (t : rest) = normalizeType t : normalizeCross rest
normalizeCross ts = ts

normalizeUnion :: [Type] -> [Type]
normalizeUnion (TUnion ts : rest) = normalizeUnion (ts ++ rest)
normalizeUnion (t : rest) = normalizeType t : normalizeUnion rest
normalizeUnion ts = ts