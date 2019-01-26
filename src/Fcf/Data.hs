{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    UndecidableInstances #-}

module Fcf.Data
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

    -- ** Lists

  , Foldr
  , UnList
  , Cons
  , type (++)
  , Filter
  , Head
  , Last
  , Tail
  , Init
  , Null
  , Length
  , Find
  , FindIndex
  , Elem
  , Lookup
  , SetIndex
  , ZipWith
  , Zip
  , Unzip
  , Cons2
  ) where

import GHC.TypeLits (Nat)
import qualified GHC.TypeLits as TL

import Fcf.Core
import Fcf.Combinators
import Fcf.Classes
import Fcf.Data.Nat
import Fcf.Utils

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

-- ** Lists

data Cons :: a -> [a] -> Exp [a]
type instance Eval (Cons a as) = a ': as

data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance Eval (Foldr f y '[]) = y
type instance Eval (Foldr f y (x ': xs)) = Eval (f x (Eval (Foldr f y xs)))

-- | N.B.: This is equivalent to a 'Foldr' flipped.
data UnList :: b -> (a -> b -> Exp b) -> [a] -> Exp b
type instance Eval (UnList y f xs) = Eval (Foldr f y xs)

data (++) :: [a] -> [a] -> Exp [a]
type instance Eval ((++) '[] ys) = ys
type instance Eval ((++) (x ': xs) ys) = x ': Eval ((++) xs ys)

data Filter :: (a -> Exp Bool) -> [a] -> Exp [a]
type instance Eval (Filter _p '[]) = '[]
type instance Eval (Filter p (a ': as)) =
  If (Eval (p a))
    (a ': Eval (Filter p as))
    (Eval (Filter p as))

data Head :: [a] -> Exp (Maybe a)
type instance Eval (Head '[]) = 'Nothing
type instance Eval (Head (a ': _as)) = 'Just a

data Last :: [a] -> Exp (Maybe a)
type instance Eval (Last '[]) = 'Nothing
type instance Eval (Last (a ': '[])) = 'Just a
type instance Eval (Last (a ': b ': as)) = Eval (Last (b ': as))

data Init :: [a] -> Exp (Maybe [a])
type instance Eval (Init '[]) = 'Nothing
type instance Eval (Init (a ': '[])) = 'Just '[]
type instance Eval (Init (a ': b ': as)) =
  Eval (Map (Cons a) =<< (Init (b ': as)))

data Tail :: [a] -> Exp (Maybe [a])
type instance Eval (Tail '[]) = 'Nothing
type instance Eval (Tail (_a ': as)) = 'Just as

data Null :: [a] -> Exp Bool
type instance Eval (Null '[]) = 'True
type instance Eval (Null (a ': as)) = 'False

data Length :: [a] -> Exp Nat
type instance Eval (Length '[]) = 0
type instance Eval (Length (a ': as)) = 1 TL.+ Eval (Length as)

data Find :: (a -> Exp Bool) -> [a] -> Exp (Maybe a)
type instance Eval (Find _p '[]) = 'Nothing
type instance Eval (Find p (a ': as)) =
  If (Eval (p a))
    ('Just a)
    (Eval (Find p as))

-- | Find the index of an element satisfying the predicate.
data FindIndex :: (a -> Exp Bool) -> [a] -> Exp (Maybe Nat)
type instance Eval (FindIndex _p '[]) = 'Nothing
type instance Eval (FindIndex p (a ': as)) =
  Eval (If (Eval (p a))
    (Pure ('Just 0))
    (Map ((+) 1) =<< FindIndex p as))

type Elem a as = IsJust =<< FindIndex (TyEq a) as

-- | Find an element associated with a key.
-- @
-- 'Lookup' :: k -> [(k, b)] -> 'Exp' ('Maybe' b)
-- @
type Lookup (a :: k) (as :: [(k, b)]) =
  (Map Snd (Eval (Find (TyEq a <=< Fst) as)) :: Exp (Maybe b))

-- | Modify an element at a given index.
--
-- The list is unchanged if the index is out of bounds.
data SetIndex :: Nat -> a -> [a] -> Exp [a]
type instance Eval (SetIndex n a' as) = SetIndexImpl n a' as

type family SetIndexImpl (n :: Nat) (a' :: k) (as :: [k]) where
  SetIndexImpl _n _a' '[] = '[]
  SetIndexImpl 0 a' (_a ': as) = a' ': as
  SetIndexImpl n a' (a ': as) = a ': SetIndexImpl (n TL.- 1) a' as

data ZipWith :: (a -> b -> Exp c) -> [a] -> [b] -> Exp [c]
type instance Eval (ZipWith _f '[] _bs) = '[]
type instance Eval (ZipWith _f _as '[]) = '[]
type instance Eval (ZipWith f (a ': as) (b ': bs)) =
  Eval (f a b) ': Eval (ZipWith f as bs)

-- |
-- @
-- 'Zip' :: [a] -> [b] -> 'Exp' [(a, b)]
-- @
type Zip = ZipWith (Pure2 '(,))

data Unzip :: Exp [(a, b)] -> Exp ([a], [b])
type instance Eval (Unzip as) = Eval (Foldr Cons2 '( '[], '[]) (Eval as))

data Cons2 :: (a, b) -> ([a], [b]) -> Exp ([a], [b])
type instance Eval (Cons2 '(a, b) '(as, bs)) = '(a ': as, b ': bs)

