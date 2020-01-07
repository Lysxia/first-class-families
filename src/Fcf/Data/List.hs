{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    UndecidableInstances #-}

-- | Lists.
module Fcf.Data.List
  ( Foldr
  , Unfoldr
  , UnList
  , Cons
  , type (++)
  , Concat
  , ConcatMap
  , Filter
  , Head
  , Last
  , Tail
  , Init
  , Null
  , Length
  , Replicate
  , Find
  , FindIndex
  , Elem
  , Lookup
  , SetIndex
  , ZipWith
  , Zip
  , Unzip
  , Cons2
  , Take
  , Drop
  , Reverse
  ) where

import qualified GHC.TypeLits as TL

import Fcf.Core
import Fcf.Combinators
import Fcf.Classes
import Fcf.Data.Common
import Fcf.Data.Nat
import Fcf.Utils

data Cons :: a -> [a] -> Exp [a]
type instance Eval (Cons a as) = a ': as

data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance Eval (Foldr f y '[]) = y
type instance Eval (Foldr f y (x ': xs)) = Eval (f x (Eval (Foldr f y xs)))

-- | N.B.: This is equivalent to a 'Foldr' flipped.
data UnList :: b -> (a -> b -> Exp b) -> [a] -> Exp b
type instance Eval (UnList y f xs) = Eval (Foldr f y xs)

-- Helper for the Unfoldr.
data UnfoldrCase :: (b -> Exp (Maybe (a, b))) -> Maybe (a, b) -> Exp [a]
type instance Eval (UnfoldrCase f ('Just ab)) =
  Eval (Fst ab) ': Eval (Unfoldr f (Eval (Snd ab)))
type instance Eval (UnfoldrCase _ 'Nothing) = '[]

-- | Type-level Unfoldr.
--
-- Example:
--
-- @
-- data ToThree :: Nat -> Exp (Maybe (Nat, Nat))
-- type instance Eval (ToThree b) =
--   If (Eval (b Fcf.>= 4))
--     'Nothing
--     ('Just '(b, b TL.+ 1))
--
-- :kind! Eval (Unfoldr ToThree 0)
-- @
--
-- See also the definition of `Replicate`.
data Unfoldr :: (b -> Exp (Maybe (a, b))) -> b -> Exp [a]
type instance Eval (Unfoldr f c) = Eval (UnfoldrCase f (f @@ c))

data (++) :: [a] -> [a] -> Exp [a]
type instance Eval ((++) '[] ys) = ys
type instance Eval ((++) (x ': xs) ys) = x ': Eval ((++) xs ys)

-- | Concat for lists.
--
-- Examples:
--
-- @
-- :kind! Eval (Concat ( '[ '[1,2], '[3,4], '[5,6]]))
-- :kind! Eval (Concat ( '[ '[Int, Maybe Int], '[Maybe String, Either Double Int]]))
-- @
data Concat :: [[a]] -> Exp [a]
type instance Eval (Concat lsts) = Eval (Foldr (++) '[] lsts)

-- | ConcatMap for lists.
data ConcatMap :: (a -> Exp [b]) -> [a] -> Exp [b]
type instance Eval (ConcatMap f lst) = Eval (Concat (Eval (Map f lst)))

data Filter :: (a -> Exp Bool) -> [a] -> Exp [a]
type instance Eval (Filter _p '[]) = '[]
type instance Eval (Filter p (a ': as)) =
  Eval (If (Eval (p a))
    ('(:) a <$> Filter p as)
    (Filter p as))

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

-- Helper for the Replicate.
data NumIter :: a -> Nat -> Exp (Maybe (a, Nat))
type instance Eval (NumIter a s) =
  If (Eval (s > 0))
    ('Just '(a, s TL.- 1))
    'Nothing


-- | Type-level `Replicate` for lists.
--
-- Example:
--
-- @
-- :kind! Eval (Replicate 4 '("ok", 2))
-- @
data Replicate :: Nat -> a -> Exp [a]
type instance Eval (Replicate n a) = Eval (Unfoldr (NumIter a) n)

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

data Elem :: a -> [a] -> Exp a
type instance Eval (Elem a as) = Eval (IsJust =<< FindIndex (TyEq a) as)

-- | Find an element associated with a key.
-- @
-- 'Lookup' :: k -> [(k, b)] -> 'Exp' ('Maybe' b)
-- @
data Lookup :: k -> [(k, b)] -> Exp (Maybe b)
type instance Eval (Lookup (a :: k) (as :: [(k, b)])) =
  Eval (Map Snd (Eval (Find (TyEq a <=< Fst) as)) :: Exp (Maybe b))

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
data Zip :: [a] -> [b] -> Exp [(a, b)]
type instance Eval (Zip as bs) = Eval (ZipWith (Pure2 '(,)) as bs)

data Unzip :: Exp [(a, b)] -> Exp ([a], [b])
type instance Eval (Unzip as) = Eval (Foldr Cons2 '( '[], '[]) (Eval as))

data Cons2 :: (a, b) -> ([a], [b]) -> Exp ([a], [b])
type instance Eval (Cons2 '(a, b) '(as, bs)) = '(a ': as, b ': bs)


-- Helper for the Take. This corresponds to the unsafeTake in the data list lib.
data UtakeStop :: Nat -> [a] -> Exp [a]
type instance Eval (UtakeStop 0 _)         = '[]
type instance Eval (UtakeStop 1 (a ': as)) = '[a]

-- Helper for the Take. This corresponds to the unsafeTake in the data list lib.
data Utake :: Nat -> [a] -> Exp [a]
-- As the following refuses to compile, we use UtakeStop-helper.
-- type instance Eval (Utake 0 (a ': as)) = '[]
-- type instance Eval (Utake 1 (a ': as)) = '[a]
-- type instance Eval (Utake m (a ': as)) = Eval ('(:) a <$> (Utake (m TL.- 1) as))
type instance Eval (Utake n (a ': as)) = Eval
  (If (Eval (n < 2))
    (UtakeStop n (a ': as))
    ('(:) a <$> (Utake (n TL.- 1) as))
  )

-- | Type-level list take.
--
-- === Example
--
-- @
-- :kind! Eval (Take 2 '[1,2,3,4,5])
-- @
data Take :: Nat -> [a] -> Exp [a]
type instance Eval (Take n as) = Eval
  (If (Eval (n > 0))
    (Utake n as)
    (Pure '[])
  )

-- Helper for the Drop. This corresponds to the unsafeDrop in the data list lib.
data UdropStop :: Nat -> [a] -> Exp [a]
type instance Eval (UdropStop 0 _ )        = '[]
type instance Eval (UdropStop 1 (a ': as)) = as
-- Helper for the Drop. This corresponds to the unsafeDrop in the data list lib.
data Udrop :: Nat -> [a] -> Exp [a]
type instance Eval (Udrop n (a ': as)) = Eval
  (If (Eval (n < 2))
    (UdropStop n (a ': as))
    (Udrop (n TL.- 1) as)
  )

-- | Type-level list drop.
--
-- === Example
--
-- @
-- :kind! Eval (Drop 2 '[1,2,3,4,5])
-- @
data Drop :: Nat -> [a] -> Exp [a]
type instance Eval (Drop n as) = Eval
  (If (Eval (n > 0))
    (Udrop n as)
    (Pure as)
  )


-- Helper for Reverse. This corresponds to rev in the data list lib.
data Rev :: [a] -> [a] -> Exp [a]
type instance Eval (Rev '[]       a) = a
type instance Eval (Rev (x ': xs) a) = Eval (Rev xs (x ': a))


-- | Type-level list reverse.
--
-- === Example
--
-- @
-- :kind! Eval (Reverse '[1,2,3,4,5])
-- @
data Reverse :: [a] -> Exp [a]
type instance Eval (Reverse l) = Eval (Rev l '[])

