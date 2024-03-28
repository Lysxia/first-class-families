{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}

-- | Carriers of useful monoid instances.
module Fcf.Class.Monoid.Types
  ( -- * Endo
    Endo(..)
  , UnEndo
  ) where

import Fcf.Core (Exp)
import Fcf.Combinators (Pure, type (<=<))
import Fcf.Class.Monoid

-- | Endofunctions.
--
-- === __Details__
--
-- This is is used in the default implementation of
-- 'Fcf.Class.Foldable.Foldr' in terms of
-- 'Fcf.Class.Foldable.FoldMap'.
newtype Endo a = Endo (a -> Exp a)

-- | Inverse of the 'Endo' constructor.
type family UnEndo (e :: Endo a) :: a -> Exp a where
  UnEndo ('Endo f) = f

-- * Endo as a monoid
--
-- Note it is only a monoid up to 'Eval'.

type instance 'Endo f <> 'Endo g = 'Endo (f <=< g)
type instance MEmpty = 'Endo Pure
