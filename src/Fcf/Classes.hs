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
  , Compare
  ) where


import Fcf.Core
import Fcf.Class.Functor


-- $
-- >>> import Fcf.Core


-- | Type-level 'Data.Bifunctor.bimap'.
data Bimap :: (a -> Exp a') -> (b -> Exp b') -> f a b -> Exp (f a' b')

-- (,)
type instance Eval (Bimap f g '(x, y)) = '(Eval (f x), Eval (g y))

-- Either
type instance Eval (Bimap f g ('Left  x)) = 'Left  (Eval (f x))
type instance Eval (Bimap f g ('Right y)) = 'Right (Eval (g y))


-- | Type-level 'Compare' for totally ordered data types.
--
-- See "Fcf.Data.Symbol" for an example.
data Compare :: a -> a -> Exp Ordering
