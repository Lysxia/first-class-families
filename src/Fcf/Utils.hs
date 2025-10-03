{-# LANGUAGE
    AllowAmbiguousTypes,
    ConstraintKinds,
    DataKinds,
    PolyKinds,
    RankNTypes,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}

-- | Miscellaneous families.
module Fcf.Utils
  ( Error
  , TError
  , Constraints
  , TyEq
  , Stuck
  , IsBool(_If)
  , Case
  , Match()
  , type (-->)
  , Is
  , Any
  , Else

    -- * From "Data.Type.Bool"
  , If

    -- * Compile-time asserts
  , Assert
  , AssertNot

    -- * Reexports
  , GHC.TypeLits.ErrorMessage(Text, ShowType, type (:<>:), type (:$$:))
  , GHC.TypeLits.TypeError
  ) where

import Data.Kind (Constraint)
import Data.Type.Bool (If)
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(..))

import Fcf.Core
import Fcf.Combinators (Pure, type (=<<))
import Fcf.Data.Bool (Not)

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
--
-- === __Details__
--
-- The base library also defines a similar @('Type.Equality.==')@;
-- it differs from 'TyEq' in the following ways:
--
-- * 'TyEq' is heterogeneous: its arguments may have different kinds;
-- * 'TyEq' is reflexive: @TyEq a a@ always reduces to 'True' even if @a@ is
--   a variable.
data TyEq :: a -> b -> Exp Bool
type instance Eval (TyEq a b) = TyEqImpl a b

type family TyEqImpl (a :: k) (b :: l) :: Bool where
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

data Match j k
  = Match_ j k
  | Is_ (j -> Exp Bool) k
  | Any_ k
  | Else_ (j -> Exp k)

-- | (Limited) equivalent of @\\case { .. }@ syntax. Supports matching of exact
-- values ('-->') and final matches for any value ('Any') or for passing value
-- to subcomputation ('Else'). Examples:
--
-- @
-- type BoolToNat = 'Case'
--   [ 'True  '-->' 0
--   , 'False '-->' 1
--   ]
--
-- type NatToBool = 'Case'
--   [ 0 '-->' 'False
--   , 'Any'   'True
--   ]
--
-- type ZeroOneOrSucc = 'Case'
--   [ 0  '-->' 0
--   , 1  '-->' 1
--   , 'Else'   (('+') 1)
--   ]
-- @
data Case :: [Match j k] -> j -> Exp k
type instance Eval (Case ms a) = Case_ ms a

type family Case_ (ms :: [Match j k]) (a :: j) :: k where
  Case_ ('Match_ a' b : ms) a = Eval (If (TyEqImpl a' a) (Pure b) (Case ms a))
  Case_ ('Is_ p b    : ms) a = Case_ [ 'True  --> b
                                     , 'False --> Case_ ms a
                                     ] (p @@ a)
  Case_ ('Any_ b     : _ ) _ = b
  Case_ ('Else_ f    : _ ) a = f @@ a

-- | Match concrete type in 'Case'.
type (-->) = ('Match_ :: j -> k -> Match j k)

-- | Match on predicate being successful with type in 'Case'.
type Is = ('Is_ :: (j -> Exp Bool) -> k -> Match j k)

-- | Match any type in 'Case'. Should be used as a final branch.
--
-- Note: this identifier conflicts with 'Fcf.Class.Foldable.Any' (from "Fcf.Class.Foldable")
-- 'Data.Monoid.Any' (from "Data.Monoid"), and 'GHC.Exts.Any' (from "GHC.Exts").
--
-- We recommend importing this one qualified.
type Any = ('Any_ :: k -> Match j k)

-- | Pass type being matched in 'Case' to subcomputation. Should be used as a
-- final branch.
type Else = ('Else_ :: (j -> Exp k) -> Match j k)

-- | A compile-time assert.
--
-- Raises the provided `TypeError`, whenever the condition evaluates to `False`.
--
-- Usage example:
-- @
-- type ExampleAssertionFailure = Eval (
--   Pure '["foo", "bar"]
--   >>= Length
--   >>= Assert ('Text "Assertion") (TyEq Int Void)
--   )
-- @
data Assert :: ErrorMessage -> Exp Bool -> r -> Exp r
type instance Eval (Assert msg mcond k)
  = Eval (If (Eval mcond) (Pure k) (TError msg))

-- | Compile-time assert, with condition negated.
--
-- Raises the provided `TypeError`, whenever the condition evaluates to `True`.
--
-- Also see 'Assert'.
data AssertNot :: forall r. ErrorMessage -> Exp Bool -> r -> Exp r
type instance Eval (AssertNot err mcond k)
  = Eval (Assert err (Not =<< mcond) k)
