{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}

-- | Simple combinators for functions.
module Fcf.Data.Function
  ( type (&)
  , On
  , Bicomap
  ) where

import Fcf.Core

infixl 1 &

-- $setup
-- >>> :set -XTypeFamilies -XDataKinds -XTypeOperators
-- >>> import Fcf.Core
-- >>> import Fcf.Combinators (Pure)
-- >>> import Fcf.Data.Common (Fst)
-- >>> import Fcf.Data.Bool (type (&&), type (||))

-- | Reverse function application, argument first.
--
-- === __Example__
--
-- >>> :kind! Eval ('(True, Nothing) & Fst)
-- Eval ('(True, Nothing) & Fst) :: Bool
-- = True
data (&) :: a -> (a -> Exp b) -> Exp b
type instance Eval (x & f) = Eval (f x)

-- | Lift a binary function to the domain of a projection.
--
-- === __Example__
--
-- >>> :kind! Eval (((&&) `On` Fst) '(True, Nothing) '(False, Just '()))
-- Eval (((&&) `On` Fst) '(True, Nothing) '(False, Just '())) :: Bool
-- = False
data On :: (b -> b -> Exp c) -> (a -> Exp b) -> a -> a -> Exp c
type instance Eval (On r f x y) = Eval (r (Eval (f x)) (Eval (f y)))

-- | Pre-compose a binary function with a function for each argument.
--
-- === __Example__
--
-- >>> :kind! Eval (Bicomap Fst Pure (||) '(False, Nothing) True)
-- Eval (Bicomap Fst Pure (||) '(False, Nothing) True) :: Bool
-- = True
data Bicomap :: (a -> Exp c) -> (b -> Exp d) -> (c -> d -> Exp e) -> a -> b -> Exp e
type instance Eval (Bicomap f g r x y) = Eval (r (Eval (f x)) (Eval (g y)))
