{-# LANGUAGE
    TypeOperators #-}

-- | Overloaded functions.
module Fcf.Classes
  {-# DEPRECATED "Use Fcf.Class.Functor or Fcf.Class.Bifunctor instead." #-}
  ( Map
  , Bimap
  ) where

import Fcf.Class.Functor
import Fcf.Class.Bifunctor
