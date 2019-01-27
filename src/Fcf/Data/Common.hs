{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators #-}

-- | Common data types: tuples, 'Either', 'Maybe'.
module Fcf.Data.Common
  ( -- ** Pairs

    Uncurry
  , Fst
  , Snd

    -- ** Either

  , UnEither
  , IsLeft
  , IsRight

    -- ** Maybe

  , UnMaybe
  , FromMaybe
  , IsNothing
  , IsJust
  ) where

import Fcf.Core

-- ** Pairs

data Uncurry :: (a -> b -> Exp c) -> (a, b) -> Exp c
type instance Eval (Uncurry f '(x, y)) = Eval (f x y)

data Fst :: (a, b) -> Exp a
type instance Eval (Fst '(a, _b)) = a

data Snd :: (a, b) -> Exp b
type instance Eval (Snd '(_a, b)) = b

infixr 3 ***

-- | Equivalent to 'Bimap' for pairs.
data (***) :: (b -> Exp c) -> (b' -> Exp c') -> (b, b') -> Exp (c, c')
type instance Eval ((***) f f' '(b, b')) = '(Eval (f b), Eval (f' b'))

-- ** Either

data UnEither :: (a -> Exp c) -> (b -> Exp c) -> Either a b -> Exp c
type instance Eval (UnEither f g ('Left  x)) = Eval (f x)
type instance Eval (UnEither f g ('Right y)) = Eval (g y)

data IsLeft :: Either a b -> Exp Bool
type instance Eval (IsLeft ('Left _a)) = 'True
type instance Eval (IsLeft ('Right _a)) = 'False

data IsRight :: Either a b -> Exp Bool
type instance Eval (IsRight ('Left _a)) = 'False
type instance Eval (IsRight ('Right _a)) = 'True

-- ** Maybe

data UnMaybe :: Exp b -> (a -> Exp b) -> Maybe a -> Exp b
type instance Eval (UnMaybe y f 'Nothing) = Eval y
type instance Eval (UnMaybe y f ('Just x)) = Eval (f x)

data FromMaybe :: k -> Maybe k -> Exp k
type instance Eval (FromMaybe a 'Nothing)   = a
type instance Eval (FromMaybe _a ('Just b)) = b

data IsNothing :: Maybe a -> Exp Bool
type instance Eval (IsNothing ('Just _a)) = 'False
type instance Eval (IsNothing 'Nothing) = 'True

data IsJust :: Maybe a -> Exp Bool
type instance Eval (IsJust ('Just _a)) = 'True
type instance Eval (IsJust 'Nothing) = 'False
