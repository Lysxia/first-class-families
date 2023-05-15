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
  , type (/=)
  )
  where

-- $setup
-- >>> :set -XDataKinds -XTypeOperators
-- >>> :m Fcf.Class.Eq.Exp Fcf.Class.Foldable Fcf.Core Fcf.Class.Functor

import Fcf.Class.Eq.Internal (EqExp, NotEqExp)
import Data.Type.Bool () -- ^ imported for haddock

infix  4  ==, /=

-- | 'Exp' version of 'Fcf.Class.Eq.Internal.==' that can be partially applied
--
-- === __Example__
--
-- >>> :kind! Eval (All ((==) 5) '[5,5,5])
-- Eval (All ((==) 5) '[5,5,5]) :: Bool
-- = 'True
--
type (==) = EqExp

-- | 'Exp' version of 'Fcf.Class.Eq.Internal./=' that can be partially applied
--
-- === __Example__
--
-- >>> :kind! Eval (Any ((/=) 5) '[6,5,5])
-- Eval (All ((/=) 5) '[6,5,5]) :: Bool
-- = 'True
--
type (/=) = NotEqExp
