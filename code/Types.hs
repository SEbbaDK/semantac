module Types where

import           Ast
import           Control.Monad       (foldM, void)
import           Control.Monad.State (MonadState (get, put), State, evalState)
import           Data.List           as List (intercalate)
import           Data.Map.Strict     as Map (Map, insert, lookup)
import           Data.Maybe          (fromMaybe)
import           Debug.Trace         (trace)
import           Loc

data Type
  = TNamed String
  | TCategory String
  | TCross [Type]
  | TUnion [Type]
  | TFunc Type Type
  | TVar TypeVar
  deriving (Eq, Ord)

instance Show Type where
    show (TNamed name)    = name
    show (TCategory name) = name
    show (TCross xs)      = intercalate " ⨯ " (fmap show xs)
    show (TUnion xs)      = intercalate " ∪ " (fmap show xs)
    show (TFunc a b)      = show a ++ " → " ++ show b
    show (TVar tv)        = show tv

fromSpec :: Spec -> Type
fromSpec SInteger    = tIntger
fromSpec SIdentifier = tIdentifier
fromSpec SSyntax     = tSyntax
fromSpec (SCustom x) = TNamed x
fromSpec (SCross xs) = TCross (fmap fromSpec xs)
fromSpec (SUnion xs) = TUnion (fmap fromSpec xs)
fromSpec (SFunc a b) = TFunc (fromSpec a) (fromSpec b)


tSyntax :: Type
tSyntax = TCategory "Syntax"

tIdentifier :: Type
tIdentifier = TCategory "Id"

tIntger :: Type
tIntger = TCategory "Int"


newtype TypeVar
  = TypeVar Int
  deriving (Eq, Ord)

instance Show TypeVar where
    show (TypeVar v) = "#" ++ reverse (intToAlphaRev v)
        where
            intToAlphaRev n | n <= 0 = []
            intToAlphaRev n =
                let (q, r) = quotRem (n - 1) 26 in
                toEnum (fromEnum 'a' + r) : intToAlphaRev q

typeVars :: Type -> [TypeVar]
typeVars (TNamed name)    = []
typeVars (TCategory name) = []
typeVars (TCross xs)      = concatMap typeVars xs
typeVars (TUnion xs)      = concatMap typeVars xs
typeVars (TVar v)         = [v]
typeVars (TFunc a b)      = typeVars a ++ typeVars b

type Substitutions = Map TypeVar Type

subst :: Substitutions -> Type -> Type
subst subs (TNamed x)    = TNamed x
subst subs (TCategory x) = TCategory x
subst subs (TCross xs)   = TCross (fmap (subst subs) xs)
subst subs (TUnion xs)   = TUnion (fmap (subst subs) xs)
subst subs (TFunc a b)   = TFunc (subst subs a) (subst subs b)
subst subs (TVar tv)     = fromMaybe (TVar tv) (Map.lookup tv subs)



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
