{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators #-}

-- | Overloaded functions.
module Fcf.Classes
  ( Map
  , Bimap
  , First
  , Second
  , Compare
  ) where


import Fcf.Core
import Fcf.Class.Functor
import Fcf.Class.Bifunctor

-- | Type-level 'Compare' for totally ordered data types.
--
-- See "Fcf.Data.Symbol" for an example.
data Compare :: a -> a -> Exp Ordering
