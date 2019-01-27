{-# LANGUAGE
    AllowAmbiguousTypes,
    ConstraintKinds,
    DataKinds,
    PolyKinds,
    RankNTypes,
    TypeFamilies,
    TypeInType,
    TypeOperators #-}

module Fcf.Utils
  ( Error
  , Constraints
  , TyEq
  , Stuck
  , IsBool(_If)
  , If
  ) where

import Data.Kind (Constraint)
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(..))

import Fcf.Core

data Error :: Symbol -> Exp a
type instance Eval (Error msg) = TypeError ('Text msg)

data TError :: ErrorMessage -> Exp a
type instance Eval (TError msg) = TypeError msg

data Constraints :: [Constraint] -> Exp Constraint
type instance Eval (Constraints '[]) = (() :: Constraint)
type instance Eval (Constraints (a ': as)) = (a, Eval (Constraints as))

data TyEq :: a -> b -> Exp Bool
type instance Eval (TyEq a b) = TyEqImpl a b

type family TyEqImpl (a :: k) (b :: k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl a b = 'False

-- | A stuck type that can be used like a type-level 'undefined'.
type family Stuck :: a

-- * Reification

class IsBool (b :: Bool) where
  _If :: ((b ~ 'True) => r) -> ((b ~ 'False) => r) -> r

instance IsBool 'True  where _If a _ = a
instance IsBool 'False where _If _ b = b

type family   If (b :: Bool) (x :: k) (y :: k) :: k
type instance If 'True   x _y = x
type instance If 'False _x  y = y
