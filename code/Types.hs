{-# LANGUAGE NamedFieldPuns #-}
module Types where

import           Ast
import           Loc
import           Control.Monad       (foldM, void)
import           Control.Monad.State (MonadState (get, put), State, evalState)
import           Data.List           as List (intercalate)
import           Data.Map.Strict     as Map (Map, insert, lookup)
import           Data.Maybe          (fromMaybe)
import           Debug.Trace         (trace)


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
    show (TVar tv)      =  show tv

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


data TypeEnv
  = TypeEnv
    { nextTypeVar_ :: TypeVar
    , subs         :: Substitutions
    , bindings     :: Map String Type
    , domains      :: [Loc Category]
    , systems      :: [Loc System]
    }

newTypeEnv :: [Loc Category] -> [Loc System] -> TypeEnv
newTypeEnv domains systems = TypeEnv
    { nextTypeVar_ = TypeVar 1
    , subs = mempty
    , bindings = mempty
    , domains
    , systems
    }

nextTypeVar :: TypeEnv -> (TypeVar, TypeEnv)
nextTypeVar tEnv =
    let TypeEnv { nextTypeVar_ = TypeVar tv } = tEnv
        nextTEnv = tEnv { nextTypeVar_ = TypeVar (tv + 1) }
    in (TypeVar tv, nextTEnv)

