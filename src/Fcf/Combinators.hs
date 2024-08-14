{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
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
  , Pure4
  , Pure5
  , Pure6
  , Pure7
  , Pure8
  , Pure9
  , type (=<<)
  , type (>>=)
  , type (<=<)
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

data Pure :: a -> Exp a
type instance Eval (Pure x) = x

data Pure1 :: (a -> b) -> a -> Exp b
type instance Eval (Pure1 f x) = f x

data Pure2 :: (a -> b -> c) -> a -> b -> Exp c
type instance Eval (Pure2 f x y) = f x y

data Pure3 :: (a -> b -> c -> d) -> a -> b -> c -> Exp d
type instance Eval (Pure3 f x y z) = f x y z

data Pure4 :: (a -> b -> c -> d -> e) -> a -> b -> c -> d -> Exp e
type instance Eval (Pure4 f w x y z) = f w x y z

data Pure5 :: (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> Exp f
type instance Eval (Pure5 f v w x y z) = f v w x y z

data Pure6 :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> Exp g
type instance Eval (Pure6 f u v w x y z) = f u v w x y z

data Pure7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> a -> b -> c -> d -> e -> f -> g -> Exp h
type instance Eval (Pure7 f t u v w x y z) = f t u v w x y z

data Pure8 :: (a -> b -> c -> d -> e -> f -> g -> h -> i) -> a -> b -> c -> d -> e -> f -> g -> h -> Exp i
type instance Eval (Pure8 f s t u v w x y z) = f s t u v w x y z

data Pure9 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> Exp j
type instance Eval (Pure9 f r s t u v w x y z) = f r s t u v w x y z

data (=<<) :: (a -> Exp b) -> Exp a -> Exp b
type instance Eval (k =<< e) = Eval (k (Eval e))

data (>>=) :: Exp a -> (a -> Exp b) -> Exp b
type instance Eval (e >>= k) = Eval (k (Eval e))

data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c
type instance Eval ((f <=< g) x) = Eval (f (Eval (g x)))

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
