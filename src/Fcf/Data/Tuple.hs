{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    UndecidableInstances #-}

{-|
Module      : Fcf.Data.Tuple
Description : Type-level tuple functions
Copyright   : (c) gspia 2020
                  Skyfold, 2023
License     : BSD
Maintainer  : gspia

= Fcf.Data.Tuple


-}

--------------------------------------------------------------------------------

module Fcf.Data.Tuple where

import           Fcf (Eval, Exp)

--------------------------------------------------------------------------------

-- For the doctests:

-- $setup
-- >>> import qualified GHC.TypeLits as TL

--------------------------------------------------------------------------------

-- | Swap
--
-- === __Example__
--
-- >>> :kind! Eval (Swap '(1, 2))
-- Eval (Swap '(1, 2)) :: (TL.Natural, TL.Natural)
-- = '(2, 1)
data Swap :: (a, b) -> Exp (b, a)
type instance Eval (Swap '(a,b)) = '(b,a)

-- | 2-tuple to allow for partial application of 2-tuple at the type level
data Tuple2 :: a -> b -> Exp (a, b)
type instance Eval (Tuple2 a b) = '(a,b)

-- | 3-tuple to allow for partial application of 3-tuple at the type level
data Tuple3 :: a -> b -> c -> Exp (a, b, c)
type instance Eval (Tuple3 a b c) = '(a,b,c)

-- | 4-tuple to allow for partial application of 4-tuple at the type level
data Tuple4 :: a -> b -> c -> d -> Exp (a, b, c, d)
type instance Eval (Tuple4 a b c d) = '(a,b,c,d)

-- | 5-tuple to allow for partial application of 4-tuple at the type level
data Tuple5 :: a -> b -> c -> d -> e -> Exp (a, b, c, d, e)
type instance Eval (Tuple5 a b c d e) = '(a,b,c,d,e)
