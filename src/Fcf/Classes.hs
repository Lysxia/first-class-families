{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators #-}

-- | Overloaded functions.
module Fcf.Classes
  ( Map
  , Bimap
  ) where

import Fcf.Core

-- | Type-level 'fmap' for type-level functors.
data Map :: (a -> Exp b) -> f a -> Exp (f b)

-- []
type instance Eval (Map f '[]) = '[]
type instance Eval (Map f (a ': as)) = Eval (f a) ': Eval (Map f as)

-- Maybe
type instance Eval (Map f 'Nothing) = 'Nothing
type instance Eval (Map f ('Just a)) = 'Just (Eval (f a))

-- Either
type instance Eval (Map f ('Left x)) = 'Left x
type instance Eval (Map f ('Right a)) = 'Right (Eval (f a))

-- Tuples
type instance Eval (Map f '(x, a)) =
  '(x, Eval (f a))
type instance Eval (Map f '(x, y, a)) =
  '(x, y, Eval (f a))
type instance Eval (Map f '(x, y, z, a)) =
  '(x, y, z, Eval (f a))
type instance Eval (Map f '(x, y, z, w, a)) =
  '(x, y, z, w, Eval (f a))

-- | Type-level 'Data.Bifunctor.bimap'.
data Bimap :: (a -> Exp a') -> (b -> Exp b') -> f a b -> Exp (f a' b')

-- (,)
type instance Eval (Bimap f g '(x, y)) = '(Eval (f x), Eval (g y))

-- Either
type instance Eval (Bimap f g ('Left  x)) = 'Left  (Eval (f x))
type instance Eval (Bimap f g ('Right y)) = 'Right (Eval (g y))
