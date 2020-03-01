{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    UndecidableInstances #-}

-- | Lists.
module Fcf.Data.List
  ( -- * Basic functions
    type (++)
  , type (==)
  , Head
  , Last
  , Tail
  , Cons
  , Snoc
  , Cons2
  , Init
  , Null
  , Length

  -- * List transformations
  , Reverse
  , Intersperse
  , Intercalate

  -- * Reducing lists
  , Foldr
  , UnList

  -- ** Special folds
  , Concat
  , ConcatMap
  , And
  , Or
  -- , Any -- this will clash with Fcf.Utils Any at Fcf-module
  , All
  , Sum

  -- * Unfolding and building
  , Unfoldr
  , Replicate

  -- * Sublists
  , Take
  , Drop
  , TakeWhile
  , DropWhile
  , Span
  , Break
  , Tails

  -- ** Predicates
  , IsPrefixOf
  , IsSuffixOf
  , IsInfixOf

  -- * Searching
  , Elem
  , Lookup
  , Find
  , Filter
  , Partition

  -- * Indexing lists
  , FindIndex
  , SetIndex

  -- * Zipping and unzipping
  , ZipWith
  , Zip
  , Unzip
  ) where

import qualified GHC.TypeLits as TL

import Fcf.Core
import Fcf.Combinators
import Fcf.Classes
import Fcf.Data.Bool
import Fcf.Data.Common
import Fcf.Data.Nat
import Fcf.Utils (If, TyEq)

-- $setup
-- >>> import Fcf.Core
-- >>> import Fcf.Combinators
-- >>> import qualified GHC.TypeLits as TL


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

-- | Equal tests for list equality.
--
-- === __Example__
--
-- >>> :kind! Eval ('[1,2,3] == '[1,2,3])
-- Eval ('[1,2,3] == '[1,2,3]) :: Bool
-- = 'True
--
-- >>> :kind! Eval ('[1,2,3] == '[1,3,2])
-- Eval ('[1,2,3] == '[1,3,2]) :: Bool
-- = 'False
data (==) :: [a] -> [a] -> Exp Bool
type instance Eval ((==) as bs) = Eval (And =<< ZipWith TyEq as bs)


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

data Cons2 :: (a, b) -> ([a], [b]) -> Exp ([a], [b])
type instance Eval (Cons2 '(a, b) '(as, bs)) = '(a ': as, b ': bs)

-- | Snoc for type-level lists.
--
-- === __Example__
--
-- >>> :kind! Eval (Snoc '[1,2,3] 4)
-- Eval (Snoc '[1,2,3] 4) :: [Nat]
-- = '[1, 2, 3, 4]
data Snoc :: [a] -> a -> Exp [a]
type instance Eval (Snoc lst a) = Eval (lst ++ '[a])


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

-- | Intersperse for type-level lists.
--
-- === __Example__
--
-- >>> :kind! Eval (Intersperse 0 '[1,2,3,4])
-- Eval (Intersperse 0 '[1,2,3,4]) :: [Nat]
-- = '[1, 0, 2, 0, 3, 0, 4]
data Intersperse :: a -> [a] -> Exp [a]
type instance Eval (Intersperse _   '[]      ) = '[]
type instance Eval (Intersperse sep (x ': xs)) = x ': Eval (PrependToAll sep xs)

-- helper for Intersperse
data PrependToAll :: a -> [a] -> Exp [a]
type instance Eval (PrependToAll _   '[]      ) = '[]
type instance Eval (PrependToAll sep (x ': xs)) = sep ': x ': Eval (PrependToAll sep xs)

-- | Intercalate for type-level lists.
--
-- === __Example__
--
-- >>> :kind! Eval (Intercalate '[", "] '[ '["Lorem"], '["ipsum"], '["dolor"] ])
-- Eval (Intercalate '[", "] '[ '["Lorem"], '["ipsum"], '["dolor"] ]) :: [TL.Symbol]
-- = '["Lorem", ", ", "ipsum", ", ", "dolor"]
data Intercalate :: [a] -> [[a]] -> Exp [a]
type instance Eval (Intercalate xs xss) = Eval (Concat =<< Intersperse xs xss)


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


-- | Give true if all of the booleans in the list are true.
--
-- === __Example__
--
-- >>> :kind! Eval (And '[ 'True, 'True])
-- Eval (And '[ 'True, 'True]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (And '[ 'True, 'True, 'False])
-- Eval (And '[ 'True, 'True, 'False]) :: Bool
-- = 'False
data And :: [Bool] -> Exp Bool
type instance Eval (And lst) = Eval (Foldr (&&) 'True lst)


-- | Type-level All.
--
-- === __Example__
--
-- >>> :kind! Eval (All (Flip (<) 6) '[0,1,2,3,4,5])
-- Eval (All (Flip (<) 6) '[0,1,2,3,4,5]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (All (Flip (<) 5) '[0,1,2,3,4,5])
-- Eval (All (Flip (<) 5) '[0,1,2,3,4,5]) :: Bool
-- = 'False
data All :: (a -> Exp Bool) -> [a] -> Exp Bool
type instance Eval (All p lst) = Eval (And =<< Map p lst)


-- | Give true if any of the booleans in the list is true.
--
-- === __Example__
--
-- >>> :kind! Eval (Or '[ 'True, 'True])
-- Eval (Or '[ 'True, 'True]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (Or '[ 'False, 'False])
-- Eval (Or '[ 'False, 'False]) :: Bool
-- = 'False
data Or :: [Bool] -> Exp Bool
type instance Eval (Or lst) = Eval (Foldr (||) 'False lst)


-- | Type-level Any.
--
-- === __Example__
--
-- >>> :kind! Eval (Any (Flip (<) 5) '[0,1,2,3,4,5])
-- Eval (Any (Flip (<) 5) '[0,1,2,3,4,5]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (Any (Flip (<) 0) '[0,1,2,3,4,5])
-- Eval (Any (Flip (<) 0) '[0,1,2,3,4,5]) :: Bool
-- = 'False
data Any :: (a -> Exp Bool) -> [a] -> Exp Bool
type instance Eval (Any p lst) = Eval (Or =<< Map p lst)


-- | Sum a Nat-list.
--
-- === __Example__
--
-- >>> :kind! Eval (Sum '[1,2,3])
-- Eval (Sum '[1,2,3]) :: Nat
-- = 6
data Sum :: [Nat] -> Exp Nat
type instance Eval (Sum ns) = Eval (Foldr (+) 0 ns)


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


-- | 'Span', applied to a predicate @p@ and a type-level list @xs@, returns a
-- type-level tuple where
-- first element is longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@ and second element is the remainder of the list:
--
-- === __Example__
--
-- >>> :kind! Eval (Span (Flip (<) 3) '[1,2,3,4,1,2,3,4])
-- Eval (Span (Flip (<) 3) '[1,2,3,4,1,2,3,4]) :: ([Nat], [Nat])
-- = '( '[1, 2], '[3, 4, 1, 2, 3, 4])
--
-- >>> :kind! Eval (Span (Flip (<) 9) '[1,2,3])
-- Eval (Span (Flip (<) 9) '[1,2,3]) :: ([Nat], [Nat])
-- = '( '[1, 2, 3], '[])
--
-- >>> :kind! Eval (Span (Flip (<) 0) '[1,2,3])
-- Eval (Span (Flip (<) 0) '[1,2,3]) :: ([Nat], [Nat])
-- = '( '[], '[1, 2, 3])
data Span :: (a -> Exp Bool) -> [a] -> Exp ([a],[a])
type instance Eval (Span p lst) = '( Eval (TakeWhile p lst), Eval (DropWhile p lst))


-- | 'Break', applied to a predicate @p@ and a type-level list @xs@, returns a
-- type-level tuple where
-- the first element is the longest prefix (possibly empty) of @xs@ of elements
-- that /do not satisfy/ @p@ and second element is the remainder of the list:
--
-- === __Example__
--
-- >>> :kind! Eval (Break (Flip (>) 3) '[1,2,3,4,1,2,3,4])
-- Eval (Break (Flip (>) 3) '[1,2,3,4,1,2,3,4]) :: ([Nat], [Nat])
-- = '( '[1, 2, 3], '[4, 1, 2, 3, 4])
--
-- >>> :kind! Eval (Break (Flip (<) 9) '[1,2,3])
-- Eval (Break (Flip (<) 9) '[1,2,3]) :: ([Nat], [Nat])
-- = '( '[], '[1, 2, 3])
--
-- >>> :kind! Eval (Break (Flip (>) 9) '[1,2,3])
-- Eval (Break (Flip (>) 9) '[1,2,3]) :: ([Nat], [Nat])
-- = '( '[1, 2, 3], '[])
data Break :: (a -> Exp Bool) -> [a] -> Exp ([a],[a])
type instance Eval (Break p lst) = Eval (Span (Not <=< p) lst)


-- | Tails
--
-- === __Example__
--
-- >>> :kind! Eval (Tails '[0,1,2,3])
-- Eval (Tails '[0,1,2,3]) :: [[Nat]]
-- = '[ '[0, 1, 2, 3], '[1, 2, 3], '[2, 3], '[3]]
data Tails :: [a] -> Exp [[a]]
type instance Eval (Tails '[]) = '[]
type instance Eval (Tails (a ': as)) = (a ': as) ': Eval (Tails as)


-- | 'IsPrefixOf' takes two type-level lists and returns true
-- iff the first list is a prefix of the second.
--
-- === __Example__
--
-- >>> :kind! Eval (IsPrefixOf '[0,1,2] '[0,1,2,3,4,5])
-- Eval (IsPrefixOf '[0,1,2] '[0,1,2,3,4,5]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (IsPrefixOf '[0,1,2] '[0,1,3,2,4,5])
-- Eval (IsPrefixOf '[0,1,2] '[0,1,3,2,4,5]) :: Bool
-- = 'False
--
-- >>> :kind! Eval (IsPrefixOf '[] '[0,1,3,2,4,5])
-- Eval (IsPrefixOf '[] '[0,1,3,2,4,5]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (IsPrefixOf '[0,1,3,2,4,5] '[])
-- Eval (IsPrefixOf '[0,1,3,2,4,5] '[]) :: Bool
-- = 'False
data IsPrefixOf :: [a] -> [a] -> Exp Bool
type instance Eval (IsPrefixOf xs ys) = IsPrefixOf_ xs ys

-- helper for IsPrefixOf
type family IsPrefixOf_ (xs :: [a]) (ys :: [a]) :: Bool where
  IsPrefixOf_ '[] _ = 'True
  IsPrefixOf_ _ '[] = 'False
  IsPrefixOf_ (x ': xs) (y ': ys) =
     Eval ((Eval (TyEq x y)) && IsPrefixOf_ xs ys)


-- | IsSuffixOf take two type-level lists and returns true
-- iff the first list is a suffix of the second.
--
-- === __Example__
--
-- >>> :kind! Eval (IsSuffixOf '[3,4,5] '[0,1,2,3,4,5])
-- Eval (IsSuffixOf '[3,4,5] '[0,1,2,3,4,5]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (IsSuffixOf '[3,4,5] '[0,1,3,2,4,5])
-- Eval (IsSuffixOf '[3,4,5] '[0,1,3,2,4,5]) :: Bool
-- = 'False
--
-- >>> :kind! Eval (IsSuffixOf '[] '[0,1,3,2,4,5])
-- Eval (IsSuffixOf '[] '[0,1,3,2,4,5]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (IsSuffixOf '[0,1,3,2,4,5] '[])
-- Eval (IsSuffixOf '[0,1,3,2,4,5] '[]) :: Bool
-- = 'False
data IsSuffixOf :: [a] -> [a] -> Exp Bool
type instance Eval (IsSuffixOf xs ys) =
  Eval (IsPrefixOf (Reverse @@ xs) (Reverse @@ ys))


-- | IsInfixOf take two type-level lists and returns true
-- iff the first list is a infix of the second.
--
-- === __Example__
--
-- >>> :kind! Eval (IsInfixOf '[2,3,4]  '[0,1,2,3,4,5,6])
-- Eval (IsInfixOf '[2,3,4]  '[0,1,2,3,4,5,6]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (IsInfixOf '[2,4,4]  '[0,1,2,3,4,5,6])
-- Eval (IsInfixOf '[2,4,4]  '[0,1,2,3,4,5,6]) :: Bool
-- = 'False
data IsInfixOf :: [a] -> [a] -> Exp Bool
type instance Eval (IsInfixOf xs ys) = Eval (Any (IsPrefixOf xs) =<< Tails ys)


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


data Find :: (a -> Exp Bool) -> [a] -> Exp (Maybe a)
type instance Eval (Find _p '[]) = 'Nothing
type instance Eval (Find p (a ': as)) =
  Eval (If (Eval (p a))
    (Pure ('Just a))
    (Find p as))


data Filter :: (a -> Exp Bool) -> [a] -> Exp [a]
type instance Eval (Filter _p '[]) = '[]
type instance Eval (Filter p (a ': as)) =
  Eval (If (Eval (p a))
    ('(:) a <$> Filter p as)
    (Filter p as))


-- | Partition
--
-- === __Example__
--
-- >>> :kind! Eval (Partition ((>=) 35) '[ 20, 30, 40, 50])
-- Eval (Partition ((>=) 35) '[ 20, 30, 40, 50]) :: ([Nat], [Nat])
-- = '( '[20, 30], '[40, 50])
data Partition :: (a -> Exp Bool) -> [a] -> Exp ([a],[a])
type instance Eval (Partition p lst) = Eval (Foldr (PartHelp p) '( '[], '[]) lst)

-- helper
data PartHelp :: (a -> Exp Bool) -> a -> ([a],[a]) -> Exp ([a],[a])
type instance Eval (PartHelp p a '(xs,ys)) =
  If (Eval (p a))
    '(a ': xs, ys)
    '(xs, a ': ys)


-- | Find the index of an element satisfying the predicate.
data FindIndex :: (a -> Exp Bool) -> [a] -> Exp (Maybe Nat)
type instance Eval (FindIndex _p '[]) = 'Nothing
type instance Eval (FindIndex p (a ': as)) =
  Eval (If (Eval (p a))
    (Pure ('Just 0))
    (Map ((+) 1) =<< FindIndex p as))


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
