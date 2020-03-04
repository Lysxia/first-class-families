{-# LANGUAGE
    CPP,
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    UndecidableInstances #-}

-- | Semigroups and monoids.
module Fcf.Class.Monoid
  ( -- * Pure type families
    -- | Nicer to use when applied explicitly.
    type (<>)
  , MEmpty

    -- * First-class families
    -- | Can be composed and passed to higher-order functions.
  , type (.<>)
  , MEmpty_
  ) where

import Fcf.Core (Exp, Eval)
import Data.Monoid (All(..), Any(..))
import Data.Type.Bool (type (&&), type (||))

#if __GLASGOW_HASKELL__ >= 802
import GHC.TypeLits (AppendSymbol)
#endif

-- $setup
-- >>> import GHC.TypeLits (Nat)

-- | Type-level semigroup composition @('Data.Semigroup.<>')@.
--
-- This is the fcf-encoding of @('<>')@.
-- To define a new semigroup, add type instances to @('<>')@.
data (.<>) :: a -> a -> Exp a
type instance Eval (x .<> y) = x <> y

-- | Type-level semigroup composition @('Data.Semigroup.<>')@.
type family (<>) (x :: a) (y :: a) :: a

-- (,)
type instance (<>) '(a1, a2) '(b1, b2) = '(a1 <> b1, a2 <> b2)

-- (,,)
type instance (<>) '(a1, a2, a3) '(b1, b2, b3) = '(a1 <> b1, a2 <> b2, a3 <> b3)

-- List
type instance (<>) '[] ys = ys
type instance (<>) (x ': xs) ys = x ': (<>) xs ys

-- Maybe
type instance (<>) 'Nothing b = b
type instance (<>) a 'Nothing = a
type instance (<>) ('Just a) ('Just b) = 'Just (a <> b)

-- Ordering
type instance (<>) 'EQ b = b
type instance (<>) a 'EQ = a
type instance (<>) 'LT _b = 'LT
type instance (<>) 'GT _b = 'GT

-- ()
type instance (<>) _a _b = '()

-- All
type instance (<>) ('All a) ('All b) = 'All (a && b)

-- Any
type instance (<>) ('Any a) ('Any b) = 'Any (a || b)

#if __GLASGOW_HASKELL__ >= 802
-- Symbol
-- | With /base >= 4.10.0.0/.
type instance (<>) x y = AppendSymbol x y
#endif

-- | Type-level monoid identity 'Data.Monoid.mempty'.
--
-- This is the fcf-encoding of 'MEmpty'.
data MEmpty_ :: Exp a
type instance Eval MEmpty_ = MEmpty

-- | Type-level monoid identity 'Data.Monoid.mempty'.
--
-- === __Examples__
--
-- >>> :kind! 'LT <> MEmpty
-- 'LT <> MEmpty :: Ordering
-- = 'LT
--
-- >>> :kind! MEmpty <> '( 'EQ, '[1, 2])
-- MEmpty <> '( 'EQ, '[1, 2]) :: (Ordering, [Nat])
-- = '( 'EQ, '[1, 2])
--
-- >>> :kind! '( 'GT, 'Just '()) <> MEmpty
-- '( 'GT, 'Just '()) <> MEmpty :: (Ordering, Maybe ())
-- = '( 'GT, 'Just '())
type family MEmpty :: a

-- (,)
type instance MEmpty = '(MEmpty, MEmpty)

-- (,,)
type instance MEmpty = '(MEmpty, MEmpty, MEmpty)

-- List
type instance MEmpty = '[]

-- Maybe
type instance MEmpty = 'Nothing

-- Ordering
type instance MEmpty = 'EQ

-- ()
type instance MEmpty = '()

-- All
type instance MEmpty = 'All 'True

-- Any
type instance MEmpty = 'Any 'False

-- Symbol
type instance MEmpty = ""
