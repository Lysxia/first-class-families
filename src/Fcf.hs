{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Fcf where

import Data.Kind (Type)

-- * First-class type families

-- | Kind of type-level expressions indexed by their result type.
type Exp a = a -> Type

-- | Expression evaluator.
type family Eval (e :: Exp a) :: a

-- ** Monadic operations

infixr 1 =<<, <=<
infixl 4 <$>, <*>

data Pure :: a -> Exp a
type instance Eval (Pure x) = x

data (=<<) :: (a -> Exp b) -> Exp a -> Exp b
type instance Eval (k =<< e) = Eval (k (Eval e))

data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c
type instance Eval ((f <=< g) x) = Eval (f (Eval (g x)))

data Join :: Exp (Exp a) -> Exp a
type instance Eval (Join e) = Eval (Eval e)

data (<$>) :: (a -> b) -> Exp a -> Exp b
type instance Eval (f <$> e) = f (Eval e)

data (<*>) :: Exp (a -> b) -> Exp a -> Exp b
type instance Eval (f <*> e) = Eval f (Eval e)

data Flip :: (a -> b -> Exp c) -> b -> a -> Exp c
type instance Eval (Flip f y x) = Eval (f x y)

data Uncurry :: (a -> b -> Exp c) -> (a, b) -> Exp c
type instance Eval (Uncurry f '(x, y)) = Eval (f x y)

data BimapPair :: (a -> Exp a') -> (b -> Exp b') -> (a, b) -> Exp (a', b')
type instance Eval (BimapPair f g '(x, y)) = '(Eval (f x), Eval (g y))

data UnEither :: (a -> Exp c) -> (b -> Exp c) -> Either a b -> Exp c
type instance Eval (UnEither f g ('Left  x)) = Eval (f x)
type instance Eval (UnEither f g ('Right y)) = Eval (g y)

data BimapEither
  :: (a -> Exp a') -> (b -> Exp b') -> Either a b -> Exp (Either a' b')
type instance Eval (BimapEither f g ('Left  x)) = 'Left  (Eval (f x))
type instance Eval (BimapEither f g ('Right y)) = 'Right (Eval (g y))

data UnMaybe :: b -> (a -> b) -> Maybe a -> Exp b
type instance Eval (UnMaybe y f 'Nothing) = y
type instance Eval (UnMaybe y f ('Just x)) = f x

data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance Eval (Foldr f y '[]) = y
type instance Eval (Foldr f y (x ': xs)) = Eval (f x (Eval (Foldr f y xs)))

data Traverse :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval (Traverse f '[]) = '[]
type instance Eval (Traverse f (x ': xs)) = Eval (f x) ': Eval (Traverse f xs)

infixr 2 ||
infixr 3 &&

data (||) :: Bool -> Bool -> Exp Bool
type instance Eval ('True || b) = 'True
type instance Eval (a || 'True) = 'True
type instance Eval ('False || b) = b
type instance Eval (a || 'False) = a

data (&&) :: Bool -> Bool -> Exp Bool
type instance Eval ('False && b) = 'False
type instance Eval (a && 'False) = 'False
type instance Eval ('True && b) = b
type instance Eval (a && 'True) = a

-- * Reification

class IsBool (b :: Bool) where
  _If :: ((b ~ 'True) => r) -> ((b ~ 'False) => r) -> r

instance IsBool 'True  where _If a _ = a
instance IsBool 'False where _If _ b = b

type family   If (b :: Bool) (x :: k) (y :: k) :: k
type instance If 'True   x _y = x
type instance If 'False _x  y = y
