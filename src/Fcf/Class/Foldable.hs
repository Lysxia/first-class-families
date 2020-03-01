{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    UndecidableInstances #-}

-- | Foldable types.
--
-- A minimal implementation of this interface is given by either 'FoldMap' or
-- 'Foldr', but a default still needs to be given explicitly for the other.
--
-- @
-- data MyType a = ... {- Some custom Foldable type -}
--
-- -- Method 1: Implement Foldr, default FoldMap.
-- type instance 'Eval' ('Foldr' f y xs) = ... {- Explicit implementation -}
-- type instance 'Eval' ('FoldMap' f xs) = 'FoldMapDefault_' f xs {- Default -}
--
-- -- Method 2: Implement FoldMap, default Foldr.
-- type instance 'Eval' ('FoldMap' f xs) = ... {- Explicit implementation -}
-- type instance 'Eval' ('Foldr' f y xs) = 'FoldrDefault_' f y xs {- Default -}
-- @
module Fcf.Class.Foldable
  ( -- * Core interface
    Foldr
  , FoldMap

    -- ** Default implementations
  , FoldMapDefault_
  , FoldrDefault_

    -- * Derived operations

    -- ** Predicates
  , And
  , Or
  , All
  , Any

    -- ** Numbers
  , Sum

    -- ** Lists
  , Concat
  , ConcatMap
  ) where

import Fcf.Core (Exp, Eval)
import Fcf.Combinators (Pure, Pure1, type (<=<))
import Fcf.Data.Function (Bicomap)
import Fcf.Class.Monoid
import Fcf.Class.Monoid.Types (Endo(..), UnEndo)
import Fcf.Data.Bool (type (&&), type (||))
import Fcf.Data.Nat (Nat, type (+))

-- $setup
-- >>> import Fcf.Combinators (Flip)
-- >>> import Fcf.Class.Ord (type (<))

-- | Type-level 'Data.Foldable.foldMap'.
data FoldMap :: (a -> Exp m) -> t a -> Exp m

-- List
type instance Eval (FoldMap f '[]) = MEmpty
type instance Eval (FoldMap f (x ': xs)) = Eval (f x) <> Eval (FoldMap f xs)

-- Maybe
type instance Eval (FoldMap f 'Nothing) = MEmpty
type instance Eval (FoldMap f ('Just x)) = Eval (f x)

-- Either
type instance Eval (FoldMap f ('Left _a)) = MEmpty
type instance Eval (FoldMap f ('Right x)) = Eval (f x)

-- | Default implementation of 'FoldMap'.
--
-- === __Usage__
--
-- To define an instance of 'FoldMap' for a custom @MyType@ for which you already have
-- an instance of 'Foldr':
--
-- @
-- type instance 'Eval' ('FoldMap' f (xs :: MyType a)) = 'FoldMapDefault_' f xs
-- @
--
-- ==== __Example__
--
-- >>> :kind! FoldMapDefault_ Pure '[ 'EQ, 'LT, 'GT ]
-- FoldMapDefault_ Pure '[ 'EQ, 'LT, 'GT ] :: Ordering
-- = 'LT
type FoldMapDefault_ f xs = Eval (Foldr (Bicomap f Pure (.<>)) MEmpty xs)

-- | Default implementation of 'Foldr'.
--
-- === __Usage__
--
-- To define an instance of 'Foldr' for a custom @MyType@ for which you already
-- have an instance of 'FoldMap':
--
-- @
-- type instance 'Eval' ('Foldr' f y (xs :: MyType a)) = 'FoldrDefault_' f y xs
-- @
--
-- ==== __Example__
--
-- >>> :kind! FoldrDefault_ (.<>) 'EQ '[ 'EQ, 'LT, 'GT ]
-- FoldrDefault_ (.<>) 'EQ '[ 'EQ, 'LT, 'GT ] :: Ordering
-- = 'LT
type FoldrDefault_ f y xs = Eval (UnEndo (Eval (FoldMap (Pure1 'Endo <=< Pure1 f) xs)) y)

-- | Right fold.
--
-- === __Example__
--
-- >>> :kind! Eval (Foldr (+) 0 '[1, 2, 3, 4])
-- Eval (Foldr (+) 0 '[1, 2, 3, 4]) :: Nat
-- = 10
data Foldr :: (a -> b -> Exp b) -> b -> t a -> Exp b

-- List
type instance Eval (Foldr f y '[]) = y
type instance Eval (Foldr f y (x ': xs)) = Eval (f x (Eval (Foldr f y xs)))

-- Maybe
type instance Eval (Foldr f y 'Nothing) = y
type instance Eval (Foldr f y ('Just x)) = Eval (f x y)

-- Either
type instance Eval (Foldr f y ('Left _a)) = y
type instance Eval (Foldr f y ('Right x)) = Eval (f x y)

-- * Derived operations

-- | Give @True@ if all of the booleans in the list are @True@.
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
data And :: t Bool -> Exp Bool
type instance Eval (And lst) = Eval (Foldr (&&) 'True lst)

-- | Whether all elements of the list satisfy a predicate.
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
data All :: (a -> Exp Bool) -> t a -> Exp Bool
type instance Eval (All p lst) = Eval (Foldr (Bicomap p Pure (&&)) 'True lst)


-- | Give @True@ if any of the booleans in the list are @True@.
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


-- | Whether any element of the list satisfies a predicate.
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
data Any :: (a -> Exp Bool) -> t a -> Exp Bool
type instance Eval (Any p lst) = Eval (Foldr (Bicomap p Pure (||)) 'False lst)


-- | Sum a @Nat@-list.
--
-- === __Example__
--
-- >>> :kind! Eval (Sum '[1,2,3])
-- Eval (Sum '[1,2,3]) :: Nat
-- = 6
data Sum :: t Nat -> Exp Nat
type instance Eval (Sum ns) = Eval (Foldr (+) 0 ns)


-- | Concatenate a collection of elements from a monoid.
--
-- === __Example__
--
-- For example, fold a list of lists.
--
-- > Concat :: [[a]] -> Exp [a]
--
-- >>> :kind! Eval (Concat ( '[ '[1,2], '[3,4], '[5,6]]))
-- Eval (Concat ( '[ '[1,2], '[3,4], '[5,6]])) :: [Nat]
-- = '[1, 2, 3, 4, 5, 6]
-- >>> :kind! Eval (Concat ( '[ '[Int, Maybe Int], '[Maybe String, Either Double Int]]))
-- Eval (Concat ( '[ '[Int, Maybe Int], '[Maybe String, Either Double Int]])) :: [*]
-- = '[Int, Maybe Int, Maybe String, Either Double Int]
--
data Concat :: t m -> Exp m
type instance Eval (Concat xs) = Eval (FoldMap Pure xs)

-- | Map a function and concatenate the results.
--
-- This is 'FoldMap' specialized to the list monoid.
data ConcatMap :: (a -> Exp [b]) -> t a -> Exp [b]
type instance Eval (ConcatMap f xs) = Eval (FoldMap f xs)
