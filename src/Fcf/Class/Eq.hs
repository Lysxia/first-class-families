{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    UndecidableInstances #-}

-- | Equality class, with the same semantics as @Data.Eq@ in base
--
-- Note, this is different from @Fcf.Utils.TyEq@ in two ways:
--
--  1. Does not require the types to have the same representation to be equal
--  2. Arguments must be of the same kind 
--
module Fcf.Class.Eq 
  ( type (==)
  , type (/=)
  )
  where

import Fcf.Class.Eq.Internal
