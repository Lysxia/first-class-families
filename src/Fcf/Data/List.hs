{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}

-- | Lists.
--
-- See also "Fcf.Class.Foldable" for additional functions.
module Fcf.Data.List
  ( -- * Basic functions
    type (++)
  , type (^++^)
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
  -- | See also "Fcf.Class.Foldable".
  , Foldr
  , UnList
  , Concat
  , ConcatMap

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
import Fcf.Class.Functor (Map)
import Fcf.Class.Monoid (type (<>))
import Fcf.Class.Foldable
import Fcf.Data.Bool
import Fcf.Data.Common
import Fcf.Data.Nat
import Fcf.Utils (If, TyEq)

-- $setup
-- >>> :set -XGADTs -XUndecidableInstances
-- >>> import Fcf.Core (Exp, Eval)
-- >>> import Fcf.Combinators
-- >>> import Fcf.Class.Foldable (Concat)
-- >>> import Fcf.Class.Monoid ()
-- >>> import Fcf.Data.Nat
-- >>> import Fcf.Utils (If, TyEq)
-- >>> import Data.Type.Ord ()
-- >>> import qualified GHC.TypeLits as TL
-- >>> import GHC.TypeLits (Nat)
-- >>> import Numeric.Natural (Natural)

-- | List catenation.
--
-- === __Example__
--
-- >>> data Example where Ex :: a -> Example  -- Hide the type of examples to avoid brittleness in different GHC versions
-- >>> :kind! Ex (Eval ([1, 2] ++ [3, 4]) :: [Natural])
-- Ex (Eval ([1, 2] ++ [3, 4]) :: [Natural]) :: Example
-- = Ex [1, 2, 3, 4]
--
data (++) :: [a] -> [a] -> Exp [a]
type instance Eval ((++) xs ys) = xs <> ys

infixr 5 ^++^
data (^++^) :: Exp [a] -> Exp [a] -> Exp [a]
type instance Eval ((^++^) xs ys) = Eval (LiftM2 (++) xs ys)

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


-- | Append an element to a list.
--
-- === __Example__
--
-- >>> :kind! Eval (Cons 1 [2, 3])
-- Eval (Cons 1 [2, 3]) :: [Natural]
-- = [1, 2, 3]
-- >>> :kind! Eval (Cons Int [Char, Maybe Double])
-- Eval (Cons Int [Char, Maybe Double]) :: [*]
-- = [Int, Char, Maybe Double]
--
data Cons :: a -> [a] -> Exp [a]
type instance Eval (Cons a as) = a ': as

-- | Append elements to two lists. Used in the definition of 'Unzip'.
data Cons2 :: (a, b) -> ([a], [b]) -> Exp ([a], [b])
type instance Eval (Cons2 '(a, b) '(as, bs)) = '(a ': as, b ': bs)

-- | Append an element to the end of a list.
--
-- === __Example__
--
-- >>> :kind! Eval (Snoc [1,2,3] 4)
-- Eval (Snoc [1,2,3] 4) :: [Natural]
-- = [1, 2, 3, 4]
data Snoc :: [a] -> a -> Exp [a]
type instance Eval (Snoc lst a) = Eval (lst ++ '[a])


-- Helper for Reverse. This corresponds to rev in the data list lib.
data Rev :: [a] -> [a] -> Exp [a]
type instance Eval (Rev '[]       ys) = ys
type instance Eval (Rev (x ': xs) ys) = Eval (Rev xs (x ': ys))


-- | Reverse a list.
--
-- === __Example__
--
-- >>> :kind! Eval (Reverse [1,2,3,4,5])
-- Eval (Reverse [1,2,3,4,5]) :: [Natural]
-- = [5, 4, 3, 2, 1]
data Reverse :: [a] -> Exp [a]
type instance Eval (Reverse l) = Eval (Rev l '[])

-- | Intersperse a separator between elements of a list.
--
-- === __Example__
--
-- >>> :kind! Eval (Intersperse 0 [1,2,3,4])
-- Eval (Intersperse 0 [1,2,3,4]) :: [Natural]
-- = [1, 0, 2, 0, 3, 0, 4]
data Intersperse :: a -> [a] -> Exp [a]
type instance Eval (Intersperse _   '[]      ) = '[]
type instance Eval (Intersperse sep (x ': xs)) = x ': Eval (PrependToAll sep xs)

-- | Helper for Intersperse
data PrependToAll :: a -> [a] -> Exp [a]
type instance Eval (PrependToAll _   '[]      ) = '[]
type instance Eval (PrependToAll sep (x ': xs)) = sep ': x ': Eval (PrependToAll sep xs)

-- | Join a list of words separated by some word.
--
-- === __Example__
--
-- >>> :kind! Eval (Intercalate '[", "] [ '["Lorem"], '["ipsum"], '["dolor"] ])
-- Eval (Intercalate '[", "] [ '["Lorem"], '["ipsum"], '["dolor"] ]) :: [TL.Symbol]
-- = ["Lorem", ", ", "ipsum", ", ", "dolor"]
data Intercalate :: [a] -> [[a]] -> Exp [a]
type instance Eval (Intercalate xs xss) = Eval (Concat =<< Intersperse xs xss)


-- | This is 'Foldr' with its argument flipped.
data UnList :: b -> (a -> b -> Exp b) -> [a] -> Exp b
type instance Eval (UnList y f xs) = Eval (Foldr f y xs)


-- Helper for the Unfoldr.
data UnfoldrCase :: (b -> Exp (Maybe (a, b))) -> Maybe (a, b) -> Exp [a]
type instance Eval (UnfoldrCase f ('Just ab)) =
  Eval (Fst ab) ': Eval (Unfoldr f (Eval (Snd ab)))
type instance Eval (UnfoldrCase _ 'Nothing) = '[]

-- | Unfold a generator into a list.
--
-- === __Example__
--
-- >>> data ToThree :: Nat -> Exp (Maybe (Nat, Nat))
-- >>> :{
-- type instance Eval (ToThree b) =
--   If (4 TL.<=? b)
--     Nothing
--     (Just '(b, b TL.+ 1))
-- :}
--
-- >>> :kind! Eval (Unfoldr ToThree 0)
-- Eval (Unfoldr ToThree 0) :: [Natural]
-- = [0, 1, 2, 3]
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

-- | Repeat the same element in a list.
--
-- === __Example__
--
-- >>> :kind! Eval (Replicate 4 '("ok", 2))
-- Eval (Replicate 4 '("ok", 2)) :: [(TL.Symbol, Natural)]
-- = ['("ok", 2), '("ok", 2), '("ok", 2), '("ok", 2)]
data Replicate :: Nat -> a -> Exp [a]
type instance Eval (Replicate n a) = Eval (Unfoldr (NumIter a) n)


-- | Take a prefix of fixed length.
--
-- === __Example__
--
-- >>> :kind! Eval (Take 2 [1,2,3,4,5])
-- Eval (Take 2 [1,2,3,4,5]) :: [Natural]
-- = [1, 2]
data Take :: Nat -> [a] -> Exp [a]
type instance Eval (Take n as) = Take_ n as

type family Take_ (n :: Nat) (xs :: [a]) :: [a] where
  Take_ 0 _         = '[]
  Take_ _ '[]       = '[]
  Take_ n (x ': xs) = x ': Take_ (n TL.- 1) xs

-- | Drop a prefix of fixed length, evaluate to the remaining suffix.
--
-- === __Example__
--
-- >>> :kind! Eval (Drop 2 [1,2,3,4,5])
-- Eval (Drop 2 [1,2,3,4,5]) :: [Natural]
-- = [3, 4, 5]
data Drop :: Nat -> [a] -> Exp [a]
type instance Eval (Drop n as) = Drop_ n as

type family Drop_ (n :: Nat) (xs :: [a]) :: [a] where
  Drop_ 0 xs        = xs
  Drop_ _ '[]       = '[]
  Drop_ n (x ': xs) = Drop_ (n TL.- 1) xs

-- | Take the longest prefix of elements satisfying a predicate.
--
-- === __Example__
--
-- >>> :kind! Eval (TakeWhile ((>=) 3) [1, 2, 3, 4, 5])
-- Eval (TakeWhile ((>=) 3) [1, 2, 3, 4, 5]) :: [Natural]
-- = [1, 2, 3]
data TakeWhile :: (a -> Exp Bool) -> [a] -> Exp [a]
type instance Eval (TakeWhile p '[]) = '[]
type instance Eval (TakeWhile p (x ': xs)) =
  Eval (If (Eval (p x))
      ('(:) x <$> TakeWhile p xs)
      (Pure '[]))

-- | Drop the longest prefix of elements satisfying a predicate,
-- evaluate to the remaining suffix.
--
-- === __Example__
--
-- :kind! Eval (DropWhile ((>=) 3) [1, 2, 3, 4, 5])
-- Eval (DropWhile ((>=) 3) [1, 2, 3, 4, 5]) :: [Natural]
-- = [4, 5]
data DropWhile :: (a -> Exp Bool) -> [a] -> Exp [a]
type instance Eval (DropWhile p '[]) = '[]
type instance Eval (DropWhile p (x ': xs)) =
  Eval (If (Eval (p x))
      (DropWhile p xs)
      (Pure (x ': xs)))


-- | 'Span', applied to a predicate @p@ and a list @xs@, returns a tuple:
-- the first component is the longest prefix (possibly empty) of @xs@ whose elements
-- satisfy @p@;
-- the second component is the remainder of the list.
--
-- See also 'TakeWhile', 'DropWhile', and 'Break'.
--
-- === __Example__
--
-- >>> :kind! Eval (Span (Flip (<) 3) [1,2,3,4,1,2])
-- Eval (Span (Flip (<) 3) [1,2,3,4,1,2]) :: ([Natural], [Natural])
-- = '([1, 2], [3, 4, 1, 2])
--
-- >>> :kind! Eval (Span (Flip (<) 9) [1,2,3])
-- Eval (Span (Flip (<) 9) [1,2,3]) :: ([Natural], [Natural])
-- = '([1, 2, 3], '[])
--
-- >>> :kind! Eval (Span (Flip (<) 0) [1,2,3])
-- Eval (Span (Flip (<) 0) [1,2,3]) :: ([Natural], [Natural])
-- = '( '[], [1, 2, 3])
data Span :: (a -> Exp Bool) -> [a] -> Exp ([a],[a])
type instance Eval (Span p lst) = '( Eval (TakeWhile p lst), Eval (DropWhile p lst))


-- | 'Break', applied to a predicate @p@ and a list @xs@, returns a tuple:
-- the first component is the longest prefix (possibly empty) of @xs@ whose elements
-- /do not satisfy/ @p@; the second component is the remainder of the list.
--
-- === __Example__
--
-- >>> :kind! Eval (Break (Flip (>) 3) [1,2,3,4,1,2])
-- Eval (Break (Flip (>) 3) [1,2,3,4,1,2]) :: ([Natural], [Natural])
-- = '([1, 2, 3], [4, 1, 2])
--
-- >>> :kind! Eval (Break (Flip (<) 9) [1,2,3])
-- Eval (Break (Flip (<) 9) [1,2,3]) :: ([Natural], [Natural])
-- = '( '[], [1, 2, 3])
--
-- >>> :kind! Eval (Break (Flip (>) 9) [1,2,3])
-- Eval (Break (Flip (>) 9) [1,2,3]) :: ([Natural], [Natural])
-- = '([1, 2, 3], '[])
data Break :: (a -> Exp Bool) -> [a] -> Exp ([a],[a])
type instance Eval (Break p lst) = Eval (Span (Not <=< p) lst)


-- | List of suffixes of a list.
--
-- === __Example__
--
-- >>> :kind! Eval (Tails [0,1,2,3])
-- Eval (Tails [0,1,2,3]) :: [[Natural]]
-- = [[0, 1, 2, 3], [1, 2, 3], [2, 3], '[3]]
data Tails :: [a] -> Exp [[a]]
type instance Eval (Tails '[]) = '[]
type instance Eval (Tails (a ': as)) = (a ': as) ': Eval (Tails as)


-- | Return @True@ when the first list is a prefix of the second.
--
-- === __Example__
--
-- >>> :kind! Eval ([0,1,2] `IsPrefixOf` [0,1,2,3,4,5])
-- Eval ([0,1,2] `IsPrefixOf` [0,1,2,3,4,5]) :: Bool
-- = True
--
-- >>> :kind! Eval ([0,1,2] `IsPrefixOf` [0,1,3,2,4,5])
-- Eval ([0,1,2] `IsPrefixOf` [0,1,3,2,4,5]) :: Bool
-- = False
--
-- >>> :kind! Eval ('[] `IsPrefixOf` [0,1,3,2,4,5])
-- Eval ('[] `IsPrefixOf` [0,1,3,2,4,5]) :: Bool
-- = True
--
-- >>> :kind! Eval ([0,1,3,2,4,5] `IsPrefixOf` '[])
-- Eval ([0,1,3,2,4,5] `IsPrefixOf` '[]) :: Bool
-- = False
data IsPrefixOf :: [a] -> [a] -> Exp Bool
type instance Eval (IsPrefixOf xs ys) = IsPrefixOf_ xs ys

-- helper for IsPrefixOf
type family IsPrefixOf_ (xs :: [a]) (ys :: [a]) :: Bool where
  IsPrefixOf_ '[] _ = 'True
  IsPrefixOf_ _ '[] = 'False
  IsPrefixOf_ (x ': xs) (y ': ys) =
     Eval ((Eval (TyEq x y)) && IsPrefixOf_ xs ys)


-- | Return @True@ when the first list is a suffix of the second.
--
-- === __Example__
--
-- >>> :kind! Eval (IsSuffixOf [3,4,5] [0,1,2,3,4,5])
-- Eval (IsSuffixOf [3,4,5] [0,1,2,3,4,5]) :: Bool
-- = True
--
-- >>> :kind! Eval (IsSuffixOf [3,4,5] [0,1,3,2,4,5])
-- Eval (IsSuffixOf [3,4,5] [0,1,3,2,4,5]) :: Bool
-- = False
--
-- >>> :kind! Eval (IsSuffixOf '[] [0,1,3,2,4,5])
-- Eval (IsSuffixOf '[] [0,1,3,2,4,5]) :: Bool
-- = True
--
-- >>> :kind! Eval (IsSuffixOf [0,1,3,2,4,5] '[])
-- Eval (IsSuffixOf [0,1,3,2,4,5] '[]) :: Bool
-- = False
data IsSuffixOf :: [a] -> [a] -> Exp Bool
type instance Eval (IsSuffixOf xs ys) =
  Eval (IsPrefixOf (Reverse @@ xs) (Reverse @@ ys))


-- | Return @True@ when the first list is contained within the second.
--
-- === __Example__
--
-- >>> :kind! Eval (IsInfixOf [2,3,4] [0,1,2,3,4,5,6])
-- Eval (IsInfixOf [2,3,4] [0,1,2,3,4,5,6]) :: Bool
-- = True
--
-- >>> :kind! Eval (IsInfixOf [2,4,4] [0,1,2,3,4,5,6])
-- Eval (IsInfixOf [2,4,4] [0,1,2,3,4,5,6]) :: Bool
-- = False
data IsInfixOf :: [a] -> [a] -> Exp Bool
type instance Eval (IsInfixOf xs ys) = Eval (Any (IsPrefixOf xs) =<< Tails ys)


-- | Return @True@ if an element is in a list.
--
-- See also 'FindIndex'.
--
-- === __Example__
--
-- >>> :kind! Eval (Elem 1 [1,2,3])
-- Eval (Elem 1 [1,2,3]) :: Bool
-- = True
-- >>> :kind! Eval (Elem 1 [2,3])
-- Eval (Elem 1 [2,3]) :: Bool
-- = False
--
data Elem :: a -> [a] -> Exp Bool
type instance Eval (Elem a as) = Eval (IsJust =<< FindIndex (TyEq a) as)

-- | Find an element associated with a key in an association list.
data Lookup :: k -> [(k, b)] -> Exp (Maybe b)
type instance Eval (Lookup (a :: k) (as :: [(k, b)])) =
  Eval (Map Snd (Eval (Find (TyEq a <=< Fst) as)) :: Exp (Maybe b))


-- | Find @Just@ the first element satisfying a predicate, or evaluate to
-- @Nothing@ if no element satisfies the predicate.
--
-- === __Example__
--
-- >>> :kind! Eval (Find (TyEq 0) [1,2,3])
-- Eval (Find (TyEq 0) [1,2,3]) :: Maybe Natural
-- = Nothing
--
-- >>> :kind! Eval (Find (TyEq 0) [1,2,3,0])
-- Eval (Find (TyEq 0) [1,2,3,0]) :: Maybe Natural
-- = Just 0
data Find :: (a -> Exp Bool) -> [a] -> Exp (Maybe a)
type instance Eval (Find _p '[]) = 'Nothing
type instance Eval (Find p (a ': as)) =
  Eval (If (Eval (p a))
    (Pure ('Just a))
    (Find p as))


-- | Keep all elements that satisfy a predicate, remove all that don't.
--
-- === __Example__
--
-- >>> :kind! Eval (Filter ((>) 3) [1,2,3,0])
-- Eval (Filter ((>) 3) [1,2,3,0]) :: [Natural]
-- = [1, 2, 0]
data Filter :: (a -> Exp Bool) -> [a] -> Exp [a]
type instance Eval (Filter _p '[]) = '[]
type instance Eval (Filter p (a ': as)) =
  Eval (If (Eval (p a))
    ('(:) a <$> Filter p as)
    (Filter p as))


-- | Split a list into one where all elements satisfy a predicate,
-- and a second where no elements satisfy it.
--
-- === __Example__
--
-- >>> :kind! Eval (Partition ((>=) 35) [20, 30, 40, 50])
-- Eval (Partition ((>=) 35) [20, 30, 40, 50]) :: ([Natural],
--                                                 [Natural])
-- = '([20, 30], [40, 50])
data Partition :: (a -> Exp Bool) -> [a] -> Exp ([a], [a])
type instance Eval (Partition p lst) = Eval (Foldr (PartHelp p) '( '[], '[]) lst)

-- | Helper for 'Partition'.
data PartHelp :: (a -> Exp Bool) -> a -> ([a],[a]) -> Exp ([a],[a])
type instance Eval (PartHelp p a '(xs,ys)) =
  If (Eval (p a))
    '(a ': xs, ys)
    '(xs, a ': ys)


-- | Find the index of an element satisfying the predicate.
--
-- === __Example__
--
-- >>> :kind! Eval (FindIndex ((<=) 3) [1,2,3,1,2,3])
-- Eval (FindIndex ((<=) 3) [1,2,3,1,2,3]) :: Maybe Natural
-- = Just 2
--
-- >>> :kind! Eval (FindIndex ((>) 0) [1,2,3,1,2,3])
-- Eval (FindIndex ((>) 0) [1,2,3,1,2,3]) :: Maybe Natural
-- = Nothing
data FindIndex :: (a -> Exp Bool) -> [a] -> Exp (Maybe Nat)
type instance Eval (FindIndex _p '[]) = 'Nothing
type instance Eval (FindIndex p (a ': as)) =
  Eval (If (Eval (p a))
    (Pure ('Just 0))
    (Map ((+) 1) =<< FindIndex p as))


-- | Modify an element at a given index.
--
-- The list is unchanged if the index is out of bounds.
--
-- === __Example__
--
-- >>> :kind! Eval (SetIndex 2 7 [1,2,3])
-- Eval (SetIndex 2 7 [1,2,3]) :: [Natural]
-- = [1, 2, 7]
data SetIndex :: Nat -> a -> [a] -> Exp [a]
type instance Eval (SetIndex n a' as) = SetIndexImpl n a' as

type family SetIndexImpl (n :: Nat) (a' :: k) (as :: [k]) where
  SetIndexImpl _n _a' '[] = '[]
  SetIndexImpl 0 a' (_a ': as) = a' ': as
  SetIndexImpl n a' (a ': as) = a ': SetIndexImpl (n TL.- 1) a' as

-- | Combine elements of two lists pairwise.
--
-- === __Example__
--
-- >>> :kind! Eval (ZipWith (+) [1,2,3] [1,1,1])
-- Eval (ZipWith (+) [1,2,3] [1,1,1]) :: [Natural]
-- = [2, 3, 4]
data ZipWith :: (a -> b -> Exp c) -> [a] -> [b] -> Exp [c]
type instance Eval (ZipWith _f '[] _bs) = '[]
type instance Eval (ZipWith _f _as '[]) = '[]
type instance Eval (ZipWith f (a ': as) (b ': bs)) =
  Eval (f a b) ': Eval (ZipWith f as bs)

data Zip :: [a] -> [b] -> Exp [(a, b)]
type instance Eval (Zip as bs) = Eval (ZipWith (Pure2 '(,)) as bs)

data Unzip :: Exp [(a, b)] -> Exp ([a], [b])
type instance Eval (Unzip as) = Eval (Foldr Cons2 '( '[], '[]) (Eval as))
