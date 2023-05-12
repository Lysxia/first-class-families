{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    StandaloneKindSignatures,
    UndecidableInstances #-}

-- | Equality class, Exp versions that can be partially applied
--
module Fcf.Class.Eq.Exp
  ( type (==)
  , type (&&)
  , type (||)
  , type (/=)
  , Not
  )
  where

import Fcf.Class.Eq.Internal (EqExp, NotEqExp, OrExp, NotExp, AndExp)

infix  4  ==, /=
infixr 3  &&
infixr 2  ||

type (==) = EqExp
type (/=) = NotEqExp
type (&&) = AndExp
type (||) = OrExp
type Not = NotExp
