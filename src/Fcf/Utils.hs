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
  , Case
  , Match
  , type (-->)
  , Any
  , Else

    -- * From "Data.Type.Bool"
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

-- * Case splitting

infix 0 -->

data Match j k = Match_ j k | Any_ k | Else_ (j -> Exp k)

-- | Equivalent (limited) of @\\case { .. }@ syntax at value level. Supports
-- matching of exact types ('-->') and final matches for any type ('Any') or
-- for passing type to subcomputation ('Else'). Examples:
--
-- @
-- type BoolToNat = Case [
--     'True  --> 0
--   , 'False --> 1
--   ]
--
-- type NatToBool = Case [
--     0 --> 'False
--   , Any   'True
--   ]
--
-- type ZeroOneOrSucc = Case [
--     0  --> 0
--   , 1  --> 1
--   , Else   ((+) 1)
--   ]
--
-- data Incr :: Nat -> Exp Nat
-- type instance Eval (Incr n) = n + 1
-- @
data Case :: [Match j k] -> j -> Exp k
type instance Eval (Case ms a) = Case_ ms a

type family Case_ (ms :: [Match j k]) (a :: j) :: k where
  Case_ ('Match_ a b : _ ) a = b
  Case_ ('Match_ _ _ : ms) a = Case_ ms a
  Case_ ('Any_ b     : _ ) _ = b
  Case_ ('Else_ f    : _ ) a = Eval (f a)

-- | Match concrete type in 'Case'.
type (-->) = 'Match_

-- | Match any type in 'Case'. Should be used as a final branch.
type Any = 'Any_

-- | Pass type being matched in 'Case' to subcomputation. Should be used as a
-- final branch.
type Else = 'Else_