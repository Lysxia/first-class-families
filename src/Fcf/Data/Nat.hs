{-# LANGUAGE
    CPP,
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif

-- | Natural numbers.
--
-- Note that the operators from this module conflict with "GHC.TypeLits" and
-- "GHC.TypeNats".
module Fcf.Data.Nat
  ( -- * Reexported type
    -- | From "GHC.TypeNats".

    Nat

    -- * Operations

  , type (+)
  , type (-)
  , type (Fcf.Data.Nat.*)
  , type (^)

    -- * Comparisons
    -- | Note that these conflict with "Fcf.Class.Ord".
  , type (<=)
  , type (>=)
  , type (<)
  , type (>)
  ) where

import GHC.TypeLits (Nat)
import qualified GHC.TypeLits as TL

import Fcf.Core
import Fcf.Combinators
import Fcf.Data.Bool (NotExp)

data (+) :: Nat -> Nat -> Exp Nat
type instance Eval ((+) a b) = a TL.+ b

data (-) :: Nat -> Nat -> Exp Nat
type instance Eval ((-) a b) = a TL.- b

data (*) :: Nat -> Nat -> Exp Nat
type instance Eval ((Fcf.Data.Nat.*) a b) = a TL.* b

data (^) :: Nat -> Nat -> Exp Nat
type instance Eval ((^) a b) = a TL.^ b

data (<=) :: Nat -> Nat -> Exp Bool
type instance Eval ((<=) a b) = a TL.<=? b

data (>=) :: Nat -> Nat -> Exp Bool
type instance Eval ((>=) a b) = b TL.<=? a

data (<) :: Nat -> Nat -> Exp Bool
type instance Eval ((<) a b) = Eval (NotExp =<< (a >= b))

data (>) :: Nat -> Nat -> Exp Bool
type instance Eval ((>) a b) = Eval (NotExp =<< (a <= b))

