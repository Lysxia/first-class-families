{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    UndecidableInstances #-}

-- | Semigroups and monoids.
module Fcf.Class.Monoid
  ( type (<>)
  , MEmpty
  , MEmpty_
  ) where

import Fcf.Core (Exp, Eval)

-- | Type-level semigroup composition @('Data.Semigroup.<>')@.
data (<>) :: a -> a -> Exp a

-- List
type instance Eval ((<>) '[] ys) = ys
type instance Eval ((<>) (x ': xs) ys) = x ': Eval ((<>) xs ys)

-- Maybe
type instance Eval ('Nothing <> b) = b
type instance Eval (a <> 'Nothing) = a
type instance Eval ('Just a <> 'Just b) = 'Just (Eval (a <> b))

-- Ordering
type instance Eval ('EQ <> b) = b
type instance Eval (a <> 'EQ) = a
type instance Eval ('LT <> _b) = 'LT
type instance Eval ('GT <> _b) = 'GT

-- | Type-level monoid identity 'Data.Monoid.mempty'.
--
-- This is the defunctionalized symbol for 'MEmpty'.
data MEmpty_ :: Exp a
type instance Eval MEmpty_ = MEmpty

-- | Type-level monoid identity 'Data.Monoid.mempty'.
type family MEmpty :: a

-- List
type instance MEmpty = '[]

-- Maybe
type instance MEmpty = 'Nothing

-- Ordering
type instance MEmpty = 'EQ
