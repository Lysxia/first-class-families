{-# LANGUAGE
    CPP,
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif


-- | Numeric operations.
module Fcf.Class.Num
  ( -- * Pure type families
    -- | Nicer to use when applied explicitly.
    type (:+)
  , type (:-)
  , type (:*)
  , type (:^)

    -- * First-class families
    -- | Can be composed and passed to higher-order functions.
  , type (+)
  , type (-)
  , type (Fcf.Class.Num.*)
  , type (^)
  ) where

import Fcf.Core (Exp, Eval)

#if __GLASGOW_HASKELL__ >= 802
import qualified GHC.TypeLits as TL
#endif

-- | Type-level addition.
--
-- This is the fcf-encoding of @(':+')@.
-- To define a new addition, add type instances to @(':+')@.
data (+) :: a -> a -> Exp a
type instance Eval (x + y) = x :+ y

-- | Type-level semigroup composition @('Data.Semigroup.<>')@.
type family (:+) (x :: a) (y :: a) :: a

type instance (:+) (a :: TL.Nat) (b :: TL.Nat)= a TL.+ b

-- | Type-level subtraction.
--
-- This is the fcf-encoding of @(':-')@.
-- To define a new subtraction, add type instances to @(':-')@.
data (-) :: a -> a -> Exp a
type instance Eval (x - y) = x :- y

-- | Type-level subtraction.
type family (:-) (x :: a) (y :: a) :: a

type instance (:-) (a :: TL.Nat) (b :: TL.Nat) = a TL.- b

-- | Type-level multiplication.
--
-- This is the fcf-encoding of @('Fcf.Data.Nat.*')@.
-- To define a new multiplication, add type instances to @('Fcf.Data.Nat.*')@.
data (*) :: a -> a -> Exp a

type instance Eval (x Fcf.Class.Num.* y) = x :* y

-- | Type-level multiplication.
type family (:*) (x :: a) (y :: a) :: a

type instance (:*) (a :: TL.Nat) (b :: TL.Nat) = a TL.* b


-- | Type-level exponentiation.
--
-- This is the fcf-encoding of @('^')@.
-- To define a new exponentiation, add type instances to @('^')@.
data (^) :: a -> b -> Exp a
type instance Eval (x ^ y) = x :^ y

-- | Type-level exponentiation.
type family (:^) (x :: a) (y :: b) :: a

type instance (:^) (a :: TL.Nat) (b :: TL.Nat) = a TL.^ b

