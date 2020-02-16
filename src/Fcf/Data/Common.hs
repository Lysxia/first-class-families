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
  , First
  , Second
  , type (***)

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

-- $setup
-- >>> import Fcf.Data.Nat
-- >>> import Fcf.Data.Symbol

-- ** Pairs

data Uncurry :: (a -> b -> Exp c) -> (a, b) -> Exp c
type instance Eval (Uncurry f '(x, y)) = Eval (f x y)

data Fst :: (a, b) -> Exp a
type instance Eval (Fst '(a, _b)) = a

data Snd :: (a, b) -> Exp b
type instance Eval (Snd '(_a, b)) = b


-- | Type-level First. Tuples @(,)@ and @Either@ have First-instances.
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

-- | Type-level Second. Tuples @(,)@ and @Either@ have Second-instances.
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
