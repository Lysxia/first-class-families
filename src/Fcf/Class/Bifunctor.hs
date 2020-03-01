{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    UndecidableInstances #-}

-- | Bifunctors.
--
-- Bifunctors are "two-argument functors".
module Fcf.Class.Bifunctor
  ( Bimap
  , First
  , Second
  ) where

import Fcf.Core (Exp, Eval, type (@@))

-- $setup
-- >>> import Fcf.Combinators (Flip)
-- >>> import Fcf.Data.Nat (Nat, type (+), type (-))
-- >>> import Fcf.Data.Symbol (Symbol)

-- | Type-level 'Data.Bifunctor.bimap'.
--
-- >>> :kind! Eval (Bimap ((+) 1) (Flip (-) 1) '(2, 4))
-- Eval (Bimap ((+) 1) (Flip (-) 1) '(2, 4)) :: (Nat, Nat)
-- = '(3, 3)
data Bimap :: (a -> Exp a') -> (b -> Exp b') -> f a b -> Exp (f a' b')

-- (,)
type instance Eval (Bimap f g '(x, y)) = '(Eval (f x), Eval (g y))

-- Either
type instance Eval (Bimap f g ('Left  x)) = 'Left  (Eval (f x))
type instance Eval (Bimap f g ('Right y)) = 'Right (Eval (g y))


-- | Type-level 'Data.Bifunctor.first'.
-- Apply a function along the first parameter of a bifunctor.
--
-- Tuples @(,)@ and @Either@ have 'First' instances.
--
-- === __Example__
--
-- >>> :kind! Eval (First ((+) 1) '(3,"a"))
-- Eval (First ((+) 1) '(3,"a")) :: (Nat, Symbol)
-- = '(4, "a")
data First :: (a -> Exp b) -> f a c -> Exp (f b c)

-- (,)
type instance Eval (First f '(a,b)) = '(f @@ a, b)

-- Either
type instance Eval (First f ('Left a)) = 'Left (f @@ a)
type instance Eval (First f ('Right b)) = 'Right b

-- | Type-level 'Data.Bifunctor.second'.
-- Apply a function along the second parameter of a bifunctor.
--
-- Tuples @(,)@ and @Either@ have 'Second' instances.
--
-- This is generally equivalent to 'Data.Functor.Map'.
--
-- === __Example__
--
-- >>> :kind! Eval (Second ((+) 1) '("a",3))
-- Eval (Second ((+) 1) '("a",3)) :: (Symbol, Nat)
-- = '("a", 4)
data Second :: (c -> Exp d) -> f a c -> Exp (f a d)

-- (,)
type instance Eval (Second f '(a,b)) = '(a, f @@ b)

-- Either
type instance Eval (Second f ('Left a)) = 'Left a
type instance Eval (Second f ('Right b)) = 'Right (f @@ b)
