{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}

{-|
Module      : Fcf.Class.Applicative
Description : Applicative definitions
Copyright   : (c) gspia, 2020
                  Skyfold, 2023
License     : BSD
Maintainer  : gspia

= Fcf.Control.Applicative

Provides a type level equivalent to Control.Applicative.

The type class hierarchy is implicit (LiftA2 defined in terms of FMap and <*>).

-}

module Fcf.Class.Applicative
  (
  -- * Applicative functors
    type (<*>)
  , Pure
  , LiftA2
  -- , type (*>)
  -- , type (<*)
  -- * Alternatives
  -- , Empty
  -- , type (<|>)
  -- , Some
  -- , Many
  -- * Instances
  -- , Const
  -- , WrappedMonad
  -- , WrappedArrow
  -- , ZipList
  -- * Utility functions
  -- , type (<$>)
  -- , type (<$)
  -- , type (<**>)
  -- , LiftA
  , LiftA3
  , LiftA4
  , LiftA5
  -- , Optional
  -- , Asum
  , App2
  , App3
  , App4
  , App5
  ) where

import           Data.Functor.Identity

import Fcf (Exp, Eval, type (.))
import Fcf.Class.Functor (FMap)
import Fcf.Class.Monoid (type (<>), MEmpty)
import Fcf.Data.List (type (++))
import qualified Fcf.Combinators as C

-- $setup
-- >>> :set -XUndecidableInstances -XTypeInType -XGADTs
-- >>> import Fcf.Core (Eval, Exp)
-- >>> import Fcf.Data.Nat
-- >>> import qualified GHC.TypeLits as TL
-- >>> import qualified Fcf.Combinators as C


--------------------------------------------------------------------------------
-- Applicative

-- | Applicative Pure.
--
-- :kind! Eval (Pure 1) :: Maybe Nat
-- :kind! Eval (Pure 1) :: Either Symbol Nat
data Pure :: a -> Exp (m a)
type instance Eval (Pure a) = '[a]
type instance Eval (Pure a) = 'Just a
type instance Eval (Pure a) = 'Right a
type instance Eval (Pure a) = 'Identity a
type instance Eval (Pure a) = '(MEmpty, a)
type instance Eval (Pure a) = '(MEmpty, MEmpty, a)
type instance Eval (Pure a) = '(MEmpty, MEmpty, MEmpty, a)


-- | (<*>) corresponds to the value level '<*>'. Note that this clashes with
-- the definition given at Fcf.Combinators.((<*>)).
--
-- Applicatives that we define include:
--
--  - Identity
--  - []
--  - Maybe
--  - Either
--  - (,)
--  - (,,)
--  - (,,,)
--
-- === __Example__
--
-- >>> :kind! Eval ('Identity Plus2 <*> 'Identity 5)
-- Eval ('Identity Plus2 <*> 'Identity 5) :: Identity Natural
-- = 'Identity 7
--
-- >>> :kind! Eval ( (<*>) '[ (Fcf.+) 1, (Fcf.*) 10] '[4,5,6,7])
-- Eval ( (<*>) '[ (Fcf.+) 1, (Fcf.*) 10] '[4,5,6,7]) :: [Natural]
-- = '[5, 6, 7, 8, 40, 50, 60, 70]
-- >>> :kind! Eval ( (<*>) '[ (Fcf.+) 1, (Fcf.*) 10] '[])
-- Eval ( (<*>) '[ (Fcf.+) 1, (Fcf.*) 10] '[]) :: [Natural]
-- = '[]
-- >>> :kind! Eval ( (<*>) '[] '[4,5,6,7])
-- Eval ( (<*>) '[] '[4,5,6,7]) :: [b]
-- = '[]
--
--
data (<*>) :: f (a -> Exp b) -> f a -> Exp (f b)

type instance Eval ('Identity f <*> m) = Eval (FMap f m)

type instance Eval ('[] <*> _) = '[]
type instance Eval (_ <*> '[]) = '[]
type instance Eval ((f ': fs) <*> (a ': as)) =
    Eval ((++) (Eval (Star_ f (a ': as))) (Eval ((<*>) fs (a ':as))))

-- Maybe
type instance Eval ('Nothing <*> _) = 'Nothing
type instance Eval ('Just f <*> m) = Eval (FMap f m)

-- Either
type instance Eval ('Left e <*> _) = 'Left e
type instance Eval ('Right f <*> m) = Eval (FMap f m)

-- | For tuples, the 'Monoid' constraint determines how the first values merge.
-- For example, 'Symbol's concatenate:
--
-- >>> :kind! Eval ('("hello", (Fcf.+) 15) <*> '("world!", 2002))
-- Eval ('("hello", (Fcf.+) 15) <*> '("world!", 2002)) :: (TL.Symbol,
--                                                         Natural)
-- = '("helloworld!", 2017)
type instance Eval ('(u, f) <*> '(v, x)) = '(u <> v, Eval (f x))

-- ((,,) a b)
type instance Eval ('(a, b, f) <*> '(a', b', x)) = '(a <> a', b <> b', Eval (f x))

-- ((,,,) a b)
type instance Eval ('(a, b, c, f) <*> '(a', b', c', x)) = '(a <> a', b <> b', c <> c', Eval (f x))

-- | Type level LiftA2.
--
-- === __Example__
--
-- >>> :kind! Eval (LiftA2 (Fcf.+) '[1,2] '[3,4])
-- Eval (LiftA2 (Fcf.+) '[1,2] '[3,4]) :: [Natural]
-- = '[4, 5, 5, 6]
--
--
data LiftA2 :: (a -> b -> Exp c) -> f a -> f b -> Exp (f c)
type instance Eval (LiftA2 f fa fb) = Eval (Eval (FMap (App2 f) fa) <*> fb)

-- | Type level LiftA3.
--
-- === __Example__
-- 
-- >>> :kind! Eval (LiftA3 Tuple3 '[1,2] '[3,4] '[5,6])
-- Eval (LiftA3 Tuple3 '[1,2] '[3,4] '[5,6]) :: [(Natural, Natural,
--                                                Natural)]
-- = '[ '(1, 3, 5), '(1, 3, 6), '(1, 4, 5), '(1, 4, 6), '(2, 3, 5),
--      '(2, 3, 6), '(2, 4, 5), '(2, 4, 6)]
--
-- >>> :kind! Eval (LiftA3 Tuple3 ('Right 5) ('Right 6) ('Left "fail"))
-- Eval (LiftA3 Tuple3 ('Right 5) ('Right 6) ('Left "fail")) :: Either
--                                                                TL.Symbol (Natural, Natural, c)
-- = 'Left "fail"
--
data LiftA3 :: (a -> b -> c -> Exp d) -> f a -> f b -> f c -> Exp (f d)
type instance Eval (LiftA3 f fa fb fc) = Eval (Eval (Eval (FMap (App3 f) fa) <*> fb) <*> fc)

-- | Type level LiftA4.
data LiftA4 :: (a -> b -> c -> d -> Exp e) -> f a -> f b -> f c -> f d -> Exp (f e)
type instance Eval (LiftA4 f fa fb fc fd) = Eval (Eval (Eval (Eval (FMap (App4 f) fa) <*> fb) <*> fc) <*> fd)

-- | Type level LiftA5.
data LiftA5 :: (a -> b -> c -> d -> e -> Exp g) -> f a -> f b -> f c -> f d -> f e -> Exp (f g)
type instance Eval (LiftA5 f fa fb fc fd fe) = Eval (Eval (Eval (Eval (Eval (FMap (App5 f) fa) <*> fb) <*> fc) <*> fd) <*> fe)

--------------------------------------------------------------------------------
-- Helper Functions

-- | Needed by LiftA2 instance to partially apply function
data App2 :: (a -> b -> c) -> a -> Exp (b -> c)
type instance Eval (App2 f a) = f a

-- | Needed by LiftA3 instance to partially apply function
data App3 :: (a -> b -> c -> d) -> a -> Exp (b -> Exp (c -> d))
type instance Eval (App3 f a) = C.Pure2 f a

-- | Needed by LiftA4 instance to partially apply function
data App4 :: (a -> b -> c -> d -> e) -> a -> Exp (b -> Exp (c -> Exp (d -> e)))
type instance Eval (App4 f a) = App3 (f a)

-- | Needed by LiftA5 instance to partially apply function
data App5 :: (a -> b -> c -> d -> e -> g) -> a -> Exp (b -> Exp (c -> Exp (d -> Exp (e -> g))))
type instance Eval (App5 f a) = App4 (f a)

-- | Helper for the [] applicative instance.
data Star_ :: (a -> Exp b) -> f a -> Exp (f b)
type instance Eval (Star_ _ '[]) = '[]
type instance Eval (Star_ f (a ': as)) =
    Eval (f a) ': Eval (Star_ f as)

-- | Helper for 'FoldlM'
data FoldlMHelper :: (b -> a -> Exp (m b)) -> a -> (b -> Exp (m b)) -> Exp (b -> Exp (m b))
type instance Eval (FoldlMHelper f a b) = C.Flip (C.>>=) b . C.Flip f a

-- | Helper for [] traverse
data ConsHelper :: (a -> Exp (f b)) -> a -> f [b] -> Exp (f [b])
type instance Eval (ConsHelper f x ys) = Eval (LiftA2 (C.Pure2 '(:)) (Eval (f x)) ys)
-- The following would need an extra import line:
-- type instance Eval (Cons_f f x ys) = Eval (LiftA2 Cons (Eval (f x)) ys)
