{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    UndecidableInstances #-}

-- | Booleans.
--
-- Note that the operations from this module conflict with
-- "Data.Type.Bool".
module Fcf.Data.Bool
  ( UnBool
  , type (||)
  , type (&&)
  , Not
  ) where

import Fcf.Core

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

-- | 'Exp' version of 'Data.Type.Bool.|| that can be partially applied
--
-- === __Example__
-- 
-- >>> :kind! Eval (Foldr (||) 'False '['False,'False,'True])
-- Eval (Foldr (||) 'False '['False,'False,'True]) :: Bool
-- = 'True
--
data (||) :: Bool -> Bool -> Exp Bool
type instance Eval ('True || b) = 'True
type instance Eval (a || 'True) = 'True
type instance Eval ('False || b) = b
type instance Eval (a || 'False) = a

-- | 'Exp' version of 'Data.Type.Bool.&& that can be partially applied
--
-- === __Example__
-- 
-- >>> :kind! Eval (Foldr (&&) 'True '['True,'False,'True])
-- Eval (Foldr (&&) 'True '['True,'False,'True]) :: Bool
-- = 'False
--
data (&&) :: Bool -> Bool -> Exp Bool
type instance Eval ('False && b) = 'False
type instance Eval (a && 'False) = 'False
type instance Eval ('True && b) = b
type instance Eval (a && 'True) = a

-- | 'Exp' version of 'Data.Type.Bool.Not that can be partially applied
--
-- === __Example__
-- 
-- >>> :kind! Eval (FMap Not '[ 'False,'False,'True])
-- Eval (FMap Not '[ 'False,'False,'True]) :: [Bool]
-- = '[ 'True, 'True, 'False]
--
data Not :: Bool -> Exp Bool
type instance Eval (Not 'True)  = 'False
type instance Eval (Not 'False) = 'True
