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

    -- ** Default implementations
  , CompareDefault_
  ) where

import qualified GHC.TypeLits as TL

import Fcf.Core
import Fcf.Class.Monoid (type (<>))  -- Semigroup Ordering
import Fcf.Utils (If)
import Fcf.Class.Eq (type (==), type (/=))

-- $setup
-- >>> :set -XDataKinds -XTypeFamilies -XUndecidableInstances -XPolyKinds
-- >>> import Fcf.Utils (TyEq, If)
-- >>> import Fcf.Core (Eval)
-- >>> import Fcf.Class.Eq (type (==))
-- >>> import Fcf.Data.Bool (type (||))

-- | Type-level 'compare' for totally ordered data types.
--
-- === __Example__
--
-- >>> :kind! Eval (Compare "a" "b")
-- Eval (Compare "a" "b") :: Ordering
-- = 'LT
--
-- >>> :kind! Eval (Compare '[1, 2, 3] '[1, 2, 3])
-- Eval (Compare '[1, 2, 3] '[1, 2, 3]) :: Ordering
-- = 'EQ
--
-- >>> :kind! Eval (Compare '[1, 3] '[1, 2])
-- Eval (Compare '[1, 3] '[1, 2]) :: Ordering
-- = 'GT
data Compare :: a -> a -> Exp Ordering

-- (,)
type instance Eval (Compare '(a1, a2) '(b1, b2)) = Eval (Compare a1 b1) <> Eval (Compare a2 b2)

-- (,,)
type instance Eval (Compare '(a1, a2, a3) '(b1, b2, b3))
  = Eval (Compare a1 b1) <> Eval (Compare a2 b2) <> Eval (Compare a3 b3)

-- Either
type instance Eval (Compare ('Left a) ('Left b)) = Eval (Compare a b)
type instance Eval (Compare ('Right a) ('Right b)) = Eval (Compare a b)
type instance Eval (Compare ('Left _a) ('Right _b)) = 'LT
type instance Eval (Compare ('Right _a) ('Left _b)) = 'GT

-- Maybe
type instance Eval (Compare 'Nothing 'Nothing) = 'EQ
type instance Eval (Compare ('Just a) ('Just b)) = Eval (Compare a b)
type instance Eval (Compare 'Nothing ('Just _b)) = 'LT
type instance Eval (Compare ('Just _a) 'Nothing) = 'GT

-- List
type instance Eval (Compare '[] '[]) = 'EQ
type instance Eval (Compare (x ': xs) (y ': ys)) = Eval (Compare x y) <> Eval (Compare xs ys)
type instance Eval (Compare '[] (_y ': _ys)) = 'LT
type instance Eval (Compare (_x ': _xs) '[]) = 'GT

-- Bool
type instance Eval (Compare (a :: Bool) a) = 'EQ
type instance Eval (Compare 'False 'True) = 'GT
type instance Eval (Compare 'True 'False) = 'GT

-- Ordering
type instance Eval (Compare (a :: Ordering) a) = 'EQ
type instance Eval (Compare 'LT 'EQ) = 'LT
type instance Eval (Compare 'LT 'GT) = 'LT
type instance Eval (Compare 'EQ 'GT) = 'LT
type instance Eval (Compare 'EQ 'LT) = 'GT
type instance Eval (Compare 'GT 'LT) = 'GT
type instance Eval (Compare 'GT 'EQ) = 'GT

-- Symbol
type instance Eval (Compare a b) = TL.CmpSymbol a b

-- Nat
type instance Eval (Compare a b) = TL.CmpNat a b

-- ()
type instance Eval (Compare (a :: ()) b) = 'EQ

-- | Default implementation of 'Compare' provided you have defined a type instance for '=='
--
-- === __Usage__
--
-- To define an instance of 'Compare' for a custom @MyType@ for which you already have
-- an instance of '==':
--
-- First: Create a type level function that can be partially appled that handles less-than for @MyType@
--
-- @
-- data Lambda :: MyType -> MyType -> 'Exp' 'Bool'
-- type instance 'Eval' (Lambda x y) = 'True
-- @
--
-- Second: Create a your type instance using 'CompareDefault_'
--
-- @
-- type instance 'Eval' ('Compare' x (y :: MyType)) = 'CompareDefault_' Lambda x y
-- @
--
-- ==== __Example__
-- 
-- >>> data Test = A | B
--
-- >>> type instance (==) (x :: Test) y = Eval (TyEq x y) -- Equality instance
--
-- >>> :{
-- >>> type family TestLessThanEq (x :: Test) (y :: Test) where -- (Optional) it is for easier pattern matching (lets us write less cases)
-- >>>   TestLessThanEq _ 'A = 'True
-- >>>   TestLessThanEq 'B _ = 'True
-- >>>   TestLessThanEq _ _ = 'False
-- >>> :}
-- >>> data CompareDefaultLambda :: Test -> Test -> Exp Bool -- lambda for CompareDefault_
-- >>> type instance Eval (CompareDefaultLambda x y) = TestLessThanEq x y
--
-- >>> type instance Eval (Compare x (y :: Test)) = CompareDefault_ CompareDefaultLambda x y
-- >>> :kind! Eval (Compare 'A 'B)
-- Eval (Compare 'A 'B) :: Ordering
-- = 'GT
--        
type CompareDefault_ :: 
     (a -> a -> Exp Bool) -- ^ function to compare if first arg is less than second arg
  -> a
  -> a 
  -> Ordering
type CompareDefault_ f x y = If (x == y) 'EQ (If (Eval (f x y)) 'LT 'GT)

-- | "Smaller than or equal to". Type-level version of @('<=')@.
--
-- === __Example__
--
-- >>> :kind! Eval ("b" <= "a")
-- Eval ("b" <= "a") :: Bool
-- = 'False
data (<=) :: a -> a -> Exp Bool
type instance Eval ((<=) a b) = Eval (Compare a b) /= 'GT

-- | "Greater than or equal to". Type-level version of @('>=')@.
--
-- === __Example__
--
-- >>> :kind! Eval ("b" >= "a")
-- Eval ("b" >= "a") :: Bool
-- = 'True
data (>=) :: a -> a -> Exp Bool
type instance Eval ((>=) a b) = Eval (Compare a b) /= 'LT

-- | "Smaller than". Type-level version of @('<')@.
--
-- === __Example__
--
-- >>> :kind! Eval ("a" < "b")
-- Eval ("a" < "b") :: Bool
-- = 'True
data (<) :: a -> a -> Exp Bool
type instance Eval ((<) a b) = Eval (Compare a b) == 'LT

-- | "Greater than". Type-level version of @('>')@.
--
-- === __Example__
--
-- >>> :kind! Eval ("b" > "a")
-- Eval ("b" > "a") :: Bool
-- = 'True
data (>) :: a -> a -> Exp Bool
type instance Eval ((>) a b) = Eval (Compare a b) == 'GT
