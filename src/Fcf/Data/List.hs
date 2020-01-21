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
  , TakeWhile
  , DropWhile
  , Reverse
  ) where

import qualified GHC.TypeLits as TL

import Fcf.Core
import Fcf.Combinators
import Fcf.Classes
import Fcf.Data.Common
import Fcf.Data.Nat
import Fcf.Utils

-- $
-- >>> import Fcf.Core
-- >>> import Fcf.Combinators
-- >>> import qualified GHC.TypeLits as TL


-- | Append an element for type-level lists.
--
-- === __Example__
--
-- >>> :kind! Eval (Cons 1 '[2, 3])
-- Eval (Cons 1 '[2, 3]) :: [Nat]
-- = '[1, 2, 3]
-- >>> :kind! Eval (Cons Int '[Char, Maybe Double])
-- Eval (Cons Int '[Char, Maybe Double]) :: [*]
-- = '[Int, Char, Maybe Double]
--
data Cons :: a -> [a] -> Exp [a]
type instance Eval (Cons a as) = a ': as

-- | Foldr for type-level lists.
--
-- === __Example__
--
-- >>> :kind! Eval (Foldr (+) 0 '[1, 2, 3, 4])
-- Eval (Foldr (+) 0 '[1, 2, 3, 4]) :: Nat
-- = 10
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
-- === __Example__
--
-- >>> data ToThree :: Nat -> Exp (Maybe (Nat, Nat))
-- >>> :{
-- type instance Eval (ToThree b) =
--   If (Eval (b Fcf.>= 4))
--     'Nothing
--     ('Just '(b, b TL.+ 1))
-- :}
--
-- >>> :kind! Eval (Unfoldr ToThree 0)
-- Eval (Unfoldr ToThree 0) :: [Nat]
-- = '[0, 1, 2, 3]
--
-- See also the definition of `Replicate`.
data Unfoldr :: (b -> Exp (Maybe (a, b))) -> b -> Exp [a]
type instance Eval (Unfoldr f c) = Eval (UnfoldrCase f (f @@ c))

-- | Type-level list catenation.
--
-- === __Example__
--
-- >>> :kind! Eval ('[1, 2] ++ '[3, 4])
-- Eval ('[1, 2] ++ '[3, 4]) :: [Nat]
-- = '[1, 2, 3, 4]
--
data (++) :: [a] -> [a] -> Exp [a]
type instance Eval ((++) '[] ys) = ys
type instance Eval ((++) (x ': xs) ys) = x ': Eval ((++) xs ys)

-- | Concat for lists.
--
-- === __Example__
--
-- >>> :kind! Eval (Concat ( '[ '[1,2], '[3,4], '[5,6]]))
-- Eval (Concat ( '[ '[1,2], '[3,4], '[5,6]])) :: [Nat]
-- = '[1, 2, 3, 4, 5, 6]
-- >>> :kind! Eval (Concat ( '[ '[Int, Maybe Int], '[Maybe String, Either Double Int]]))
-- Eval (Concat ( '[ '[Int, Maybe Int], '[Maybe String, Either Double Int]])) :: [*]
-- = '[Int, Maybe Int, Maybe String, Either Double Int]
--
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
-- === __Example__
--
-- >>> :kind! Eval (Replicate 4 '("ok", 2))
-- Eval (Replicate 4 '("ok", 2)) :: [(TL.Symbol, Nat)]
-- = '[ '("ok", 2), '("ok", 2), '("ok", 2), '("ok", 2)]
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

-- | Type-level `Elem` for lists.
--
-- === __Example__
--
-- >>> :kind! Eval (Elem 1 '[1,2,3])
-- Eval (Elem 1 '[1,2,3]) :: Bool
-- = 'True
-- >>> :kind! Eval (Elem 1 '[2,3])
-- Eval (Elem 1 '[2,3]) :: Bool
-- = 'False
--
data Elem :: a -> [a] -> Exp Bool
type instance Eval (Elem a as) = Eval (IsJust =<< FindIndex (TyEq a) as)

-- | Find an element associated with a key.
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

data Zip :: [a] -> [b] -> Exp [(a, b)]
type instance Eval (Zip as bs) = Eval (ZipWith (Pure2 '(,)) as bs)

data Unzip :: Exp [(a, b)] -> Exp ([a], [b])
type instance Eval (Unzip as) = Eval (Foldr Cons2 '( '[], '[]) (Eval as))

data Cons2 :: (a, b) -> ([a], [b]) -> Exp ([a], [b])
type instance Eval (Cons2 '(a, b) '(as, bs)) = '(a ': as, b ': bs)


-- | Type-level list take.
--
-- === __Example__
--
-- >>> :kind! Eval (Take 2 '[1,2,3,4,5])
-- Eval (Take 2 '[1,2,3,4,5]) :: [Nat]
-- = '[1, 2]
data Take :: Nat -> [a] -> Exp [a]
type instance Eval (Take n as) = Take_ n as

type family Take_ (n :: Nat) (xs :: [a]) :: [a] where
  Take_ 0 _         = '[]
  Take_ _ '[]       = '[]
  Take_ n (x ': xs) = x ': Take_ (n TL.- 1) xs

-- | Type-level list drop.
--
-- === __Example__
--
-- >>> :kind! Eval (Drop 2 '[1,2,3,4,5])
-- Eval (Drop 2 '[1,2,3,4,5]) :: [Nat]
-- = '[3, 4, 5]
data Drop :: Nat -> [a] -> Exp [a]
type instance Eval (Drop n as) = Drop_ n as

type family Drop_ (n :: Nat) (xs :: [a]) :: [a] where
  Drop_ 0 xs        = xs
  Drop_ _ '[]       = '[]
  Drop_ n (x ': xs) = Drop_ (n TL.- 1) xs

-- | Type-level list takeWhile.
--
-- === __Example__
--
-- >>> :kind! Eval (TakeWhile ((>=) 3) '[1, 2, 3, 4, 5])
-- Eval (TakeWhile ((>=) 3) '[1, 2, 3, 4, 5]) :: [Nat]
-- = '[1, 2, 3]
data TakeWhile :: (a -> Exp Bool) -> [a] -> Exp [a]
type instance Eval (TakeWhile p '[]) = '[]
type instance Eval (TakeWhile p (x ': xs)) =
  Eval (If (Eval (p x))
      ('(:) x <$> TakeWhile p xs)
      (Pure '[]))

-- | Type-level list dropWhile.
--
-- === __Example__
--
-- :kind! Eval (DropWhile ((>=) 3) '[1, 2, 3, 4, 5])
-- Eval (DropWhile ((>=) 3) '[1, 2, 3, 4, 5]) :: [Nat]
-- = '[4, 5]
data DropWhile :: (a -> Exp Bool) -> [a] -> Exp [a]
type instance Eval (DropWhile p '[]) = '[]
type instance Eval (DropWhile p (x ': xs)) =
  Eval (If (Eval (p x))
      (DropWhile p xs)
      (Pure (x ': xs)))


-- Helper for Reverse. This corresponds to rev in the data list lib.
data Rev :: [a] -> [a] -> Exp [a]
type instance Eval (Rev '[]       ys) = ys
type instance Eval (Rev (x ': xs) ys) = Eval (Rev xs (x ': ys))


-- | Type-level list reverse.
--
-- === __Example__
--
-- >>> :kind! Eval (Reverse '[1,2,3,4,5])
-- Eval (Reverse '[1,2,3,4,5]) :: [Nat]
-- = '[5, 4, 3, 2, 1]
data Reverse :: [a] -> Exp [a]
type instance Eval (Reverse l) = Eval (Rev l '[])

