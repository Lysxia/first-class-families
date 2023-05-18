{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    StandaloneKindSignatures,
    UndecidableInstances #-}

-- | Booleans.
--
-- Note that the operations from this module conflict with
-- "Data.Type.Bool".
module Fcf.Data.Bool
  ( -- * Booleans
    type (||)
  , type (&&)
  , Not
    -- * Booleans, can be partially applied
  , And
  , Or
  , NotExp
    -- ** Utility
  , UnBool
  ) where

import Fcf.Core
import Data.Bool () -- ^ imported for haddock

-- $setup
-- >>> :set -XDataKinds -XTypeOperators
-- >>> :m Fcf.Class.Eq.Exp Fcf.Class.Foldable Fcf.Core Fcf.Class.Functor

-- | N.B.: The order of the two branches is the opposite of "if":
-- @UnBool ifFalse ifTrue bool@.
--
-- This mirrors the default order of constructors:
--
-- @
-- data Bool = False | True
-- ----------- False < True
-- @
data UnBool :: Exp a -> Exp a -> Bool -> Exp a
type instance Eval (UnBool fal tru 'False) = Eval fal
type instance Eval (UnBool fal tru 'True ) = Eval tru

infixr 2 ||
infixr 3 &&

-- | 
-- @
-- type __(||)__ :: 'Bool' -> 'Bool' -> 'Bool'
-- @
--
-- Type-level version of 'Data.Bool.||'
--
-- === __Example__
--
-- >>> :kind! 'True || 'False
-- 'True || 'False :: Bool
-- = 'True
--
type (||) :: Bool -> Bool -> Bool
type family (||) a b where
  'False || b = b
  'True || _ = 'True

-- | 'Exp' version of '(||)' that can be partially applied
--
-- === __Example__
-- 
-- >>> :kind! Eval (Foldr Or 'False '['False,'False,'True])
-- Eval (Foldr Or 'False '['False,'False,'True]) :: Bool
-- = 'True
--
data Or :: Bool -> Bool -> Exp Bool 
type instance Eval (Or a b) = a || b


-- | 
-- @
-- type __(&&)__ :: 'Bool' -> 'Bool' -> 'Bool'
-- @
--
-- Type-level version of 'Data.Bool.&&'
--
-- === __Example__
--
-- >>> :kind! 'True && 'False
-- 'True && 'False :: Bool
-- = 'False
--
type (&&) :: Bool -> Bool -> Bool
type family (&&) a b where
  'True && b = b
  'False && _ = 'False

-- | 'Exp' version of '(&&)' that can be partially applied
--
-- === __Example__
-- 
-- >>> :kind! Eval (Foldr And 'True '['True,'False,'True])
-- Eval (Foldr And 'True '['True,'False,'True]) :: Bool
-- = 'False
--
data And :: Bool -> Bool -> Exp Bool
type instance Eval (And a b) = a && b


-- | 
-- @
-- type __Not__ :: 'Bool' -> 'Bool'
-- @
--
-- Type-level version of 'not'
--
-- === __Example__
--
-- >>> :kind! Not 'True || 'False
-- Not 'True || 'False :: Bool
-- = 'False
--
type Not :: Bool -> Bool
type family Not (a :: Bool) :: Bool where 
  Not 'True = 'False
  Not 'False = 'True

-- | 'Exp' version of 'Not' that can be partially applied
--
-- === __Example__
-- 
-- >>> :kind! Eval (FMap Not '[ 'False,'False,'True])
-- Eval (FMap Not '[ 'False,'False,'True]) :: [Bool]
-- = '[ 'True, 'True, 'False]
--
data NotExp :: Bool -> Exp Bool
type instance Eval (NotExp a) = Not a
