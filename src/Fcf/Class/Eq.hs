{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    StandaloneKindSignatures,
    UndecidableInstances #-}

-- | Equality class
--
module Fcf.Class.Eq 
  ( -- * Equality
    type (==)
  , type (/=)
    -- * Equality, can be partially applied
  , EqExp
  , NotEq
  ) where

import Data.Type.Bool
import Fcf.Core
import Data.Eq () -- ^ imported for haddock
import qualified Data.Type.Equality as Type (type (==))
import Fcf.Data.Symbol (Symbol)
import Fcf.Data.Nat (Nat)
import Data.Kind (Type)

-- $setup
-- >>> import Fcf.Class.Foldable (All, Any)

infix 4 ==, /=

-- | 
-- @
-- type __(==)__ :: a -> a -> 'Bool'
-- @
--
-- Type-level version of 'Data.Eq.=='
-- 
-- === __Example__
-- 
-- >>> :kind! "hello" == "hello"
-- "hello" == "hello" :: Bool
-- = 'True
--
-- >>> :kind! 5 == 6
-- 5 == 6 : Bool
-- = 'False
--
type (==) :: a -> a -> Bool
type family (==) (x :: a) (y :: a) :: Bool

-- | 
-- @
-- type __(/=)__ :: a -> a -> 'Bool'
-- @
--
-- Type-level version of 'Data.Eq./='
--
-- === __Example__
-- 
-- >>> :kind! "hello" /= "hello"
-- "hello" /= "hello" :: Bool
-- = 'False
--
-- >>> :kind! 5 /= 6
-- 5 /= 6 : Bool
-- = 'True
--
type (/=) :: a -> a -> Bool
type (/=) a b = Not (a == b)

-- | 'Exp' version of '==' that can be partially applied
--
-- === __Example__
--
-- >>> :kind! Eval (All (EqExp 5) '[5,5,5])
-- Eval (All (EqExp 5) '[5,5,5]) :: Bool
-- = 'True
--
data EqExp :: a -> a -> Exp Bool
type instance Eval (EqExp a b) = a == b

-- | 'Exp' version of '/=' that can be partially applied
--
-- === __Example__
--
-- >>> :kind! Eval (Any (NotEq 5) '[6,5,5])
-- Eval (All (NotEq 5) '[6,5,5]) :: Bool
-- = 'True
--
data NotEq :: a -> a -> Exp Bool
type instance Eval (NotEq a b) = a /= b

-- (,)
type instance (==) '(a1, a2) '(b1, b2) = a1 == b1 && a2 == b2

-- (,,)
type instance (==) '(a1, a2, a3) '(b1, b2, b3) = a1 == b1 && a2 == b2 && a3 == b3

-- (,,,)
type instance (==) '(a1, a2, a3, a4) '(b1, b2, b3, b4) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4

-- (,,,,)
type instance (==) '(a1, a2, a3, a4, a5) '(b1, b2, b3, b4, b5) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5

-- List
type instance (==) '[] '[] = 'True
type instance (==) (x ': xs) (y ': ys) = x == y && xs == ys
type instance (==) (_ ': _) '[] = 'False
type instance (==) '[] (_ ': _) = 'False

-- Maybe
type instance (==) 'Nothing 'Nothing = 'True
type instance (==) ('Just _) 'Nothing = 'False 
type instance (==) 'Nothing ('Just _) = 'False 
type instance (==) ('Just x) ('Just y) = x == y

-- Either
type instance (==) ('Left x) ('Left y) = x == y
type instance (==) ('Right x) ('Right y) = x == y
type instance (==) ('Left _) ('Right y) = 'False
type instance (==) ('Right _) ('Left _) = 'False

-- ()
type instance (==) '() '() = 'True

-- Ordering
type instance (==) (x :: Ordering) (y :: Ordering) = x Type.== y

-- Symbol
type instance (==) (x :: Symbol) (y :: Symbol) = x Type.== y 

-- Nat
type instance (==) (x :: Nat) (y :: Nat) = x Type.== y

-- Bool
type instance (==) (x :: Bool) (y :: Bool) = x Type.== y

-- Type
type instance (==) (x :: Type) (y :: Type) = x Type.== y
