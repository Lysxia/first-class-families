{-# LANGUAGE
    AllowAmbiguousTypes,
    ConstraintKinds,
    DataKinds,
    PolyKinds,
    RankNTypes,
    TypeFamilies,
    TypeInType,
    TypeOperators #-}

-- | Miscellaneous families.
module Fcf.Utils
  ( Error
  , TError
  , Constraints
  , TyEq
  , Stuck
  , IsBool(_If)

    -- | From "Data.Type.Bool".
  , If
  ) where

import Data.Kind (Constraint)
import Data.Type.Bool (If)
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(..))

import Fcf.Core

-- | Type-level 'error'.
data Error :: Symbol -> Exp a
type instance Eval (Error msg) = TypeError ('Text msg)

-- | 'TypeError' as a fcf.
data TError :: ErrorMessage -> Exp a
type instance Eval (TError msg) = TypeError msg

-- | Conjunction of a list of constraints.
data Constraints :: [Constraint] -> Exp Constraint
type instance Eval (Constraints '[]) = (() :: Constraint)
type instance Eval (Constraints (a ': as)) = (a, Eval (Constraints as))

-- | Type equality.
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
