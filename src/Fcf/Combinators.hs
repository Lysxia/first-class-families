{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    UndecidableInstances #-}

-- | General fcf combinators.
--
-- See also "Fcf.Data.Function" for more.
module Fcf.Combinators
  ( Pure
  , Pure1
  , Pure2
  , Pure3
  , type (=<<)
  , type (>>=)
  , type (<=<)
  , type (.)
  , LiftM
  , LiftM2
  , LiftM3
  , Join
  , type (<$>)
  , type (<*>)
  , Flip
  , ConstFn
  , type ($)
  ) where

import Fcf.Core

-- ** Monadic operations

infixl 1 >>=
infixr 1 =<<, <=<
infixl 4 <$>, <*>
infixr 9 .

data Pure :: a -> Exp a
type instance Eval (Pure x) = x

data Pure1 :: (a -> b) -> a -> Exp b
type instance Eval (Pure1 f x) = f x

data Pure2 :: (a -> b -> c) -> a -> b -> Exp c
type instance Eval (Pure2 f x y) = f x y

data Pure3 :: (a -> b -> c -> d) -> a -> b -> c -> Exp d
type instance Eval (Pure3 f x y z) = f x y z

data (=<<) :: (a -> Exp b) -> Exp a -> Exp b
type instance Eval (k =<< e) = Eval (k (Eval e))

data (>>=) :: Exp a -> (a -> Exp b) -> Exp b
type instance Eval (e >>= k) = Eval (k (Eval e))

data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c
type instance Eval ((f . g) x) = Eval (f (Eval (g x)))

type (.) = (<=<)

type LiftM = (=<<)

data LiftM2 :: (a -> b -> Exp c) -> Exp a -> Exp b -> Exp c
type instance Eval (LiftM2 f x y) = Eval (f (Eval x) (Eval y))

data LiftM3 :: (a -> b -> c -> Exp d) -> Exp a -> Exp b -> Exp c -> Exp d
type instance Eval (LiftM3 f x y z) = Eval (f (Eval x) (Eval y) (Eval z))

data Join :: Exp (Exp a) -> Exp a
type instance Eval (Join e) = Eval (Eval e)

data (<$>) :: (a -> b) -> Exp a -> Exp b
type instance Eval (f <$> e) = f (Eval e)

data (<*>) :: Exp (a -> b) -> Exp a -> Exp b
type instance Eval (f <*> e) = Eval f (Eval e)

data Flip :: (a -> b -> Exp c) -> b -> a -> Exp c
type instance Eval (Flip f y x) = Eval (f x y)

data ConstFn :: a -> b -> Exp a
type instance Eval (ConstFn a _b) = a

-- | Note that this denotes the identity function, so @($) f@ can usually be
-- replaced with @f@.
data ($) :: (a -> Exp b) -> a -> Exp b
type instance Eval (($) f a) = Eval (f a)

infixr 0 $
