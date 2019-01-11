{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    UndecidableInstances #-}

module Fcf.Data.Nat
  ( -- * Reexported type
    -- | From "GHC.TypeNats".

    Nat

    -- * Operations

  , type (+)
  , type (-)
  , type (Fcf.Data.Nat.*)
  , type (<=)
  , type (>=)
  , type (<)
  , type (>)
  ) where

import GHC.TypeLits (Nat)
import qualified GHC.TypeLits as TL

import Fcf.Core
import Fcf.Combinators
import Fcf.Data.Bool (Not)

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
type instance Eval ((<) a b) = Eval (Not =<< (a >= b))

data (>) :: Nat -> Nat -> Exp Bool
type instance Eval ((>) a b) = Eval (Not =<< (a <= b))

