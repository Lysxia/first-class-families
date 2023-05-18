{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    StandaloneKindSignatures,
    UndecidableInstances #-}

-- | Ordering.
--
module Fcf.Class.Ord
  ( -- * Order
    Compare
  , type (<=)
  , type (>=)
  , type (<)
  , type (>)
    -- * Order, can be partially applied
  , CompareExp
  , LessThanEq
  , GreaterThanEq
  , LessThan
  , GreaterThan

    -- ** Default implementations
  , EqualityDefault_
  ) where

import qualified GHC.TypeLits as TL

import Fcf.Core
import Fcf.Class.Monoid (type (<>))  -- Semigroup Ordering
import Fcf.Class.Eq (type (==), type (/=))
import Data.Ord () -- ^ imported for haddock

-- $setup
-- >>> import Fcf.Core (Eval)
-- >>> import Fcf.Class.Eq (type (==), EqExp)
-- >>> import Fcf.Combinators (type (<=<))
-- >>> import Fcf.Functor (FMap)
-- >>> import Fcf.Data.List (Filter)

-- |
-- @
-- type __Compare__ :: a -> a -> 'Ordering'
-- @
--
-- Type-level 'compare' for totally ordered data types.
--
-- === __Example__
--
-- >>> :kind! Eval (Compare "a" "b")
-- Eval (Compare "a" "b") :: Ordering
-- = 'LT
--
-- >>> :kind! Compare '[1, 2, 3] '[1, 2, 3]
-- Compare '[1, 2, 3] '[1, 2, 3] :: Ordering
-- = 'EQ
--
-- >>> :kind! Compare '[1, 3] '[1, 2]
-- Compare '[1, 3] '[1, 2] :: Ordering
-- = 'GT
type Compare :: a -> a -> Ordering
type family Compare (x :: a) (y :: a) :: Ordering

-- | 'Exp' version of 'Compare' that can be partially applied
--
-- === __Example__
--
-- >>> :kind! Eval (FMap (CompareExp 5) '[1,9,4,5])
-- Eval (FMap (CompareExp 5) '[1,9,4,5]) :: [Ordering]
-- = '['GT,'LT,'GT,'EQ]
data CompareExp :: a -> a -> Exp Ordering
type instance Eval (CompareExp x y) = Compare x y

-- (,)
type instance (Compare '(a1, a2) '(b1, b2)) = Compare a1 b1 <> Compare a2 b2

-- (,,)
type instance (Compare '(a1, a2, a3) '(b1, b2, b3))
  = Compare a1 b1 <> Compare a2 b2 <> Compare a3 b3

-- Either
type instance (Compare ('Left a) ('Left b)) = Compare a b
type instance (Compare ('Right a) ('Right b)) = Compare a b
type instance (Compare ('Left _a) ('Right _b)) = 'LT
type instance (Compare ('Right _a) ('Left _b)) = 'GT

-- Maybe
type instance (Compare 'Nothing 'Nothing) = 'EQ
type instance (Compare ('Just a) ('Just b)) = Compare a b
type instance (Compare 'Nothing ('Just _b)) = 'LT
type instance (Compare ('Just _a) 'Nothing) = 'GT

-- List
type instance (Compare '[] '[]) = 'EQ
type instance (Compare (x ': xs) (y ': ys)) = Compare x y <> Compare xs ys
type instance (Compare '[] (_y ': _ys)) = 'LT
type instance (Compare (_x ': _xs) '[]) = 'GT

-- Bool
type instance (Compare (a :: Bool) a) = 'EQ
type instance (Compare 'False 'True) = 'GT
type instance (Compare 'True 'False) = 'GT

-- Ordering
type instance (Compare (a :: Ordering) a) = 'EQ
type instance (Compare 'LT 'EQ) = 'LT
type instance (Compare 'LT 'GT) = 'LT
type instance (Compare 'EQ 'GT) = 'LT
type instance (Compare 'EQ 'LT) = 'GT
type instance (Compare 'GT 'LT) = 'GT
type instance (Compare 'GT 'EQ) = 'GT

-- Symbol
type instance (Compare a b) = TL.CmpSymbol a b

-- Nat
type instance (Compare a b) = TL.CmpNat a b

-- ()
type instance (Compare (a :: ()) b) = 'EQ

-- | 
-- @
-- type __EqualityDefault___ :: a -> a -> 'Bool'
-- @
--
-- Default implementation of '==' provided you have defined a type instance for 'Compare'
--
-- === __Usage__
--
-- To define an instance of '==' for a custom @MyType@ for which you already have
-- an instance of 'Compare:
--
-- @
-- type instance ('==') x (y :: MyType) = 'EqualityDefault_' x y
-- @
--
-- ==== __Example__
-- 
-- >>> data Test = A | B
--
-- >>> type instance (Compare 'A 'A) = 'EQ
-- >>> type instance (Compare 'A 'B) = 'GT
-- >>> type instance (Compare 'B 'B) = 'EQ
-- >>> type instance (Compare 'B 'A) = 'LT
-- >>> type instance (==) x (y :: Test) = EqualityDefault_ x y
-- >>> :kind! 'A == 'B
-- 'A == 'B :: Bool
-- = 'False
--        
type EqualityDefault_ :: a -> a -> Bool
type EqualityDefault_ x y = Compare x y == 'EQ

-- |
-- @
-- type __(<=)__ :: a -> a -> 'Bool'
-- @
--
-- "Smaller than or equal to". Type-level version of @('Data.Ord.<=')@.
--
-- === __Example__
--
-- >>> :kind! "b" <= "a"
-- "b" <= "a" :: Bool
-- = 'False
type (<=) :: a -> a -> Bool
type (<=) a b = Compare a b /= 'GT

-- | 'Exp' version of @'<='@ that can be partially applied
--
-- === __Example__
--
-- >>> :kind! Filter (LessThanEq 5) '[1,5,9]
-- Filter (LessThanEq 5) '[1,5,9] :: [Nat]
-- = '[1,5]
data LessThanEq :: a -> a -> Exp Bool
type instance Eval (LessThanEq x y) = x <= y

-- |
-- @
-- type __(>=)__ :: a -> a -> 'Bool'
-- @
--
-- "Greater than or equal to". Type-level version of @('Data.Ord.>=')@.
--
-- === __Example__
--
-- >>> :kind! "b" >= "a"
-- "b" >= "a" :: Bool
-- = 'True
type (>=) :: a -> a -> Bool
type (>=) a b = Compare a b /= 'LT

-- | 'Exp' version of @'>='@ that can be partially applied
--
-- === __Example__
--
-- >>> :kind! Filter (GreaterThanEq 5) '[1,5,9]
-- Filter (GreaterThanEq 5) '[1,5,9] :: [Nat]
-- = '[5,9]
data GreaterThanEq :: a -> a -> Exp Bool
type instance Eval (GreaterThanEq x y) = x >= y

-- | 
-- @
-- type __(<)__ :: a -> a -> 'Bool'
-- @
--
-- "Smaller than". Type-level version of @('Data.Ord.<')@.
--
-- === __Example__
--
-- >>> :kind! "a" < "b"
-- "a" < "b" :: Bool
-- = 'True
type (<) :: a -> a -> Bool
type (<) a b = Compare a b == 'LT

-- | 'Exp' version of @'<'@ that can be partially applied
--
-- === __Example__
--
-- >>> :kind! Filter (LessThan 5) '[1,5,9]
-- Filter (LessThan 5) '[1,5,9] :: [Nat]
-- = '[1]
data LessThan :: a -> a -> Exp Bool
type instance Eval (LessThan x y) = x < y

-- | 
-- @
-- type __(>)__ :: a -> a -> Exp 'Bool'
-- @
--
-- "Greater than". Type-level version of @('Data.Ord.>')@.
--
-- === __Example__
--
-- >>> :kind! "b" > "a"
-- "b" > "a" :: Bool
-- = 'True
type (>) :: a -> a -> Bool
type (>) a b = Compare a b == 'GT

-- | 'Exp' version of @'>'@ that can be partially applied
--
-- === __Example__
--
-- >>> :kind! Filter (GreaterThan 5) '[1,5,9]
-- Filter (GreaterThan 5) '[1,5,9] :: [Nat]
-- = '[9]
data GreaterThan :: a -> a -> Exp Bool
type instance Eval (GreaterThan x y) = x > y 
