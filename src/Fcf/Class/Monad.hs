{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}

{-|
Module      : Fcf.Class.Monad
Description : Monad definitions
Copyright   : (c) gspia, 2020
                  Skyfold, 2023
License     : BSD
Maintainer  : gspia

= Fcf.Control.Monad

Provides a type level equivalent to Control.Monad and Data.Traversable. The 
modules are merged since they both depend on the Monad typeclass. There is
no way to specify a kind typeclass, so we cannot create a GHC.Base equivalent.

The type class hierarchy is implicit (LiftA2 defined in terms of fmap and <*>).

-}

module Fcf.Class.Monad 
    ( 
    -- * Functor and monad classes
      FMap
    -- , type (<$)
    , type (>>=)
    , type (>>)
    , Return
    -- , MonadFail
    -- , MonadPlus
    -- * Funtions
    , MapM
    , ForM
    , Sequence
    -- , type (=<<)
    -- , type (>=>)
    -- , type (<=<)
    -- , Void
    -- * Generalisations of list functions 
    -- , Join
    -- , Msum
    -- , Mfulter
    -- , MapAndUnzipM
    -- , ZipWithM
    , FoldlM
    -- , ReplicateM
    -- * Conditional execution of monadic expressions 
    -- , Guard
    -- , When
    -- , Unless
    -- , LiftM
    -- , LiftM2
    -- , LiftM3
    -- , LiftM4
    -- , LiftM5
    -- , Ap
    -- * The Traversable class
    , Traverse
    -- , SequenceA
    -- * Utility functions
    -- , For
    -- , MapAccumL
    -- , MapAccumR
    -- * General definitions for superclass methods
    -- , FmapDefault
    -- , FoldMapDefault
    -- * Utility
    , Id
    )
  where

import           Data.Functor.Identity
import           GHC.TypeNats as TN

import           Fcf
import qualified Fcf.Combinators as C
import           Fcf.Class.Functor (FMap)
import           Fcf.Class.Applicative
import           Fcf.Data.Tuple

--------------------------------------------------------------------------------

-- For the doctests:

-- $setup
-- >>> import qualified GHC.TypeLits as TL
-- >>> import qualified Fcf.Combinators as C

--------------------------------------------------------------------------------
-- Common methods for both Applicative and Monad


-- | Return = Pure 
--
-- :kind! Eval (Return 1) :: Maybe Nat
-- :kind! Eval (Return 1) :: Either Symbol Nat
type Return = Pure

--------------------------------------------------------------------------------
-- Monad


-- | Type level Bind corresponding to the value level bind '>>=' operator.
--  Note that name (>>=) clashes with the definition given at
--  Fcf.Combinators.(>>=). (It doesn't export it yet, though.)
--
-- Monads that we define include:
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
-- Example: double the length of the input list and increase the numbers at
-- the same time.
--
-- >>> :kind! Eval ('[5,6,7] >>= Plus2M)
-- Eval ('[5,6,7] >>= Plus2M) :: [Natural]
-- = '[7, 8, 8, 9, 9, 10]
--
--
-- >>> :kind! Eval (XsPlusYsMonadic '[1,2,3] '[4,5,6])
-- Eval (XsPlusYsMonadic '[1,2,3] '[4,5,6]) :: [Natural]
-- = '[5, 6, 7, 6, 7, 8, 7, 8, 9]
data (>>=) :: m a -> (a -> Exp (m b)) -> Exp (m b)

-- Maybe
type instance Eval ('Nothing >>= f) = 'Nothing
type instance Eval ('Just a >>= f) = Eval (f a)

-- Either
type instance Eval ('Left a >>= _) = 'Left a
type instance Eval ('Right a >>= f) = Eval (f a)

-- Identity
type instance Eval ('Identity a >>= f) = Eval (f a)

-- Lists
type instance Eval ('[] >>= _) = '[]
type instance Eval ((x ': xs) >>= f) = Eval ((f @@ x) ++  Eval (xs >>= f))

-- (,)
type instance Eval ('(u, a) >>= k) = Eval ('(u, Id) <*> Eval (k a))

-- (,,)
type instance Eval ('(u, v, a) >>= k) = Eval ('(u, v, Id) <*> Eval (k a))

-- (,,,)
type instance Eval ('(u, v, w, a) >>= k) = Eval ('(u, v, w, Id) <*> Eval (k a))

-- | Type level >> 
--
-- === __Example__
--
--
-- >>> :kind! Eval ( 'Just 1 >> 'Just 2)
-- Eval ( 'Just 1 >> 'Just 2) :: Maybe Natural
-- = 'Just 2
-- >>> :kind! Eval ( 'Nothing >> 'Just 2)
-- Eval ( 'Nothing >> 'Just 2) :: Maybe Natural
-- = 'Nothing
--
data (>>) :: m a -> m b -> Exp (m b)
type instance Eval (m >> k) = Eval (m >>= ConstFn k)

--------------------------------------------------------------------------------
-- MapM

-- | MapM
--
-- === __Example__
--
-- >>> :kind! Eval (MapM (ConstFn '[ 'True, 'False]) '["a","b","c"])
-- Eval (MapM (ConstFn '[ 'True, 'False]) '["a","b","c"]) :: [[Bool]]
-- = '[ '[ 'True, 'True, 'True], '[ 'True, 'True, 'False],
--      '[ 'True, 'False, 'True], '[ 'True, 'False, 'False],
--      '[ 'False, 'True, 'True], '[ 'False, 'True, 'False],
--      '[ 'False, 'False, 'True], '[ 'False, 'False, 'False]]
--
--
data MapM :: (a -> Exp (m b)) -> t a -> Exp (m (t b))
type instance Eval (MapM f ta) = Eval (Sequence (FMap f @@ ta))
-- Above one is same as:
-- type instance Eval (MapM f ta) = Eval (Sequence (Eval (Map f ta)))


-- | ForM = Flip MapM
data ForM :: t a -> (a -> Exp (m b)) -> Exp (m (t b))
type instance Eval (ForM ta f) = Eval (MapM f ta)


--------------------------------------------------------------------------------
-- FoldlM

-- | FoldlM
--
-- === __Example__
--
-- >>> import GHC.TypeLits as TL (Symbol, type (-))
-- >>> data Lambda :: Nat -> Nat -> Exp (Either Symbol Natural)
-- >>> type instance Eval (Lambda a b) = If (Eval (a >= b)) ('Right (a TL.- b)) ('Left "Nat cannot be negative")
-- >>> :kind! Eval (FoldlM Lambda 5 '[1,1,1])
-- Eval (FoldlM Lambda 5 '[1,1,1]) :: Either Symbol Natural
-- = 'Right 2
-- >>> :kind! Eval (FoldlM Lambda 5 '[1,4,1])
-- Eval (FoldlM Lambda 5 '[1,4,1]) :: Either Symbol Natural
-- = 'Left "Nat cannot be negative"
--
data FoldlM :: (b -> a -> Exp (m b)) -> b -> t a -> Exp (m b)
type instance Eval (FoldlM f z0 xs) = Eval ((Eval (Foldr (FoldlMHelper f) Return xs)) z0)

--------------------------------------------------------------------------------
-- Traversable


-- | Traverse
--
-- === __Example__
--
-- >>> :kind! Eval (Traverse Id '[ '[1,2], '[3,4]])
-- Eval (Traverse Id '[ '[1,2], '[3,4]]) :: [[Natural]]
-- = '[ '[1, 3], '[1, 4], '[2, 3], '[2, 4]]
--
--
data Traverse :: (a -> Exp (f b)) -> t a -> Exp (f (t b))
-- type instance Eval (Traverse f ta) = Eval (SequenceA =<< Map f ta)
-- Could the above one just be made to work? At the moment, the computation
-- diverges (we need to give the Traverse instances).

-- []
type instance Eval (Traverse f lst) =
    Eval (Foldr (ConsHelper f) (Eval (Return '[])) lst)

-- Maybe
type instance Eval (Traverse f 'Nothing) = Eval (Return 'Nothing)
type instance Eval (Traverse f ('Just x)) = Eval (FMap (C.Pure1 'Just) (Eval (f x)))

-- Either
type instance Eval (Traverse f ('Left e)) = Eval (Return ('Left e))
type instance Eval (Traverse f ('Right x)) = Eval (FMap (C.Pure1 'Right) (Eval (f x)))

-- ((,) a)
type instance Eval (Traverse f '(x, y)) = Eval (FMap (Tuple2 x) (Eval (f y)))


-- | Sequence
--
-- === __Example__
--
-- >>> :kind! Eval (Sequence ('Just ('Right 5)))
-- Eval (Sequence ('Just ('Right 5))) :: Either a (Maybe Natural)
-- = 'Right ('Just 5)
--
-- >>> :kind! Eval (Sequence '[ 'Just 3, 'Just 5, 'Just 7])
-- Eval (Sequence '[ 'Just 3, 'Just 5, 'Just 7]) :: Maybe [Natural]
-- = 'Just '[3, 5, 7]
--
-- >>> :kind! Eval (Sequence '[ 'Just 3, 'Nothing, 'Just 7])
-- Eval (Sequence '[ 'Just 3, 'Nothing, 'Just 7]) :: Maybe [Natural]
-- = 'Nothing
--
-- >>> :kind! Eval (Sequence '[ '[1,2], '[3,4]])
-- Eval (Sequence '[ '[1,2], '[3,4]]) :: [[Natural]]
-- = '[ '[1, 3], '[1, 4], '[2, 3], '[2, 4]]
--
--
data Sequence :: t (f a) -> Exp (f (t a))
type instance Eval (Sequence tfa) = Eval (Traverse Id tfa)

--------------------------------------------------------------------------------
-- Utility

-- | Id function correspondes to term level 'id'-function.
data Id :: a -> Exp a
type instance Eval (Id a) = a

--------------------------------------------------------------------------------
-- Helper Functions

-- | Helper for 'FoldlM'
data FoldlMHelper :: (b -> a -> Exp (m b)) -> a -> (b -> Exp (m b)) -> Exp (b -> Exp (m b))
type instance Eval (FoldlMHelper f a b) = Flip (>>=) b . Flip f a

-- | Helper for [] traverse
data ConsHelper :: (a -> Exp (f b)) -> a -> f [b] -> Exp (f [b])
type instance Eval (ConsHelper f x ys) = Eval (LiftA2 (C.Pure2 '(:)) (Eval (f x)) ys)
-- The following would need an extra import line:
-- type instance Eval (Cons_f f x ys) = Eval (LiftA2 Cons (Eval (f x)) ys)


--------------------------------------------------------------------------------
-- For Examples

-- | For Applicative documentation example
data Plus1 :: Nat -> Exp Nat
type instance Eval (Plus1 n) = n TN.+ 1

-- | For Applicative documentation example
data Plus2 :: Nat -> Exp Nat
type instance Eval (Plus2 n) = n TN.+ 2

-- | For the example. Turn an input number to list of two numbers of a bit
-- larger numbers.
data Plus2M :: Nat -> Exp [Nat]
type instance Eval (Plus2M n) = '[n TN.+ 2, n TN.+3]

-- | Part of an example
data PureXPlusY :: Nat -> Nat -> Exp [Nat]
type instance Eval (PureXPlusY x y) = Eval (Return ((TN.+) x y))

-- | Part of an example
data XPlusYs :: Nat -> [Nat] -> Exp [Nat]
type instance Eval (XPlusYs x ys) = Eval (ys >>= PureXPlusY x)

-- | An example implementing
--
-- sumM xs ys = do
--     x <- xs
--     y <- ys
--     return (x + y)
--
-- or
--
-- sumM xs ys = xs >>= (\x -> ys >>= (\y -> pure (x+y)))
--
-- Note the use of helper functions. This is a bit awkward, a type level
-- lambda would be nice.
data XsPlusYsMonadic :: [Nat] -> [Nat] -> Exp [Nat]
type instance Eval (XsPlusYsMonadic xs ys) = Eval (xs >>= Flip XPlusYs ys)

-- data Sumnd :: [Nat] -> [Nat] -> Exp [Nat]
-- type instance Eval (Sumnd xs ys) = xs >>=

-- data Sum2 :: Nat -> Nat -> Exp Nat
-- type instance Eval (Sum2 x y) = x TN.+ y

