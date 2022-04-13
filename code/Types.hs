{-# LANGUAGE NamedFieldPuns #-}
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
  = TInteger
  | TIdentifier
  | TSyntax
  | TCustom String
  | TCross [Type]
  | TUnion [Type]
  | TFunc Type Type
  | TVar TypeVar
  deriving (Eq, Ord)

instance Show Type where
    show TInteger       = "Int"
    show TIdentifier    = "Id"
    show TSyntax        = "Syntax"
    show (TCustom name) = name
    show (TCross xs)    = "<" ++ intercalate " , " (fmap show xs) ++ ">"
    show (TUnion xs)    = "(" ++ intercalate " | " (fmap show xs) ++ ")"
    show (TFunc a b)    = show a ++ " → " ++ show b
    show (TVar tv)      = show tv

fromSpec :: Spec -> Type
fromSpec Integer    = TInteger
fromSpec Identifier = TIdentifier
fromSpec SSyntax    = TSyntax
fromSpec (Custom x) = TCustom x
fromSpec (Cross xs) = TCross (fmap fromSpec xs)
fromSpec (Union xs) = TUnion (fmap fromSpec xs)
fromSpec (Func a b) = TFunc (fromSpec a) (fromSpec b)

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

type Substitutions = Map TypeVar Type

subst :: Substitutions -> Type -> Type
subst subs TInteger    = TInteger
subst subs TIdentifier = TIdentifier
subst subs TSyntax     = TSyntax
subst subs (TCustom x) = TCustom x
subst subs (TCross xs) = TCross (fmap (subst subs) xs)
subst subs (TUnion xs) = TUnion (fmap (subst subs) xs)
subst subs (TFunc a b) = TFunc (subst subs a) (subst subs b)
subst subs (TVar tv)   = fromMaybe (TVar tv) (Map.lookup tv subs)



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
