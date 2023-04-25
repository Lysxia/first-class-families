{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}

{-|
Module      : Fcf.Data.Traversable
Description : Class of data structures that can be traversed from left to right, performing an action on each element.
Copyright   : (c) gspia, 2020
                  Skyfold, 2023
License     : BSD
Maintainer  : gspia

= Fcf.Data.Traversable

Provides a type level equivalent to Data.Traversable without any Monad dependencise.

The type class hierarchy is implicit (Traverse defined in terms of FMap and Foldr).

-}

module Fcf.Data.Traversable 
    ( Traverse
    , SequenceA
    )
  where

-- import           Data.Functor.Identity

import           Fcf
import qualified Fcf.Combinators as C
import           Fcf.Class.Functor (FMap)
import           Fcf.Class.Applicative
import           Fcf.Data.Tuple
import           Fcf.Data.Function (Id)

--------------------------------------------------------------------------------
-- Traversable

-- | Traverse
--
-- === __Example__
--
-- >>> :kind! Eval (Traverse Id '[ '[1,2], '[3,4]])
-- Eval (Traverse Id '[ '[1,2], '[3,4]]) :: [[Natural]]
-- = '[ '[1, 3], '[1, 4], '[2, 3], '[2, 4]]
--
--
data Traverse :: (a -> Exp (f b)) -> t a -> Exp (f (t b))
-- type instance Eval (Traverse f ta) = Eval (SequenceA =<< Map f ta)
-- Could the above one just be made to work? At the moment, the computation
-- diverges (we need to give the Traverse instances).

-- []
type instance Eval (Traverse f lst) =
    Eval (Foldr (ConsHelper f) (Eval (Pure '[])) lst)

-- Maybe
type instance Eval (Traverse f 'Nothing) = Eval (Pure 'Nothing)
type instance Eval (Traverse f ('Just x)) = Eval (FMap (C.Pure1 'Just) (Eval (f x)))

-- Either
type instance Eval (Traverse f ('Left e)) = Eval (Pure ('Left e))
type instance Eval (Traverse f ('Right x)) = Eval (FMap (C.Pure1 'Right) (Eval (f x)))

-- ((,) a)
type instance Eval (Traverse f '(x, y)) = Eval (FMap (Tuple2 x) (Eval (f y)))

-- | Sequence
--
-- === __Example__
--
-- >>> :kind! Eval (Sequence ('Just ('Right 5)))
-- Eval (Sequence ('Just ('Right 5))) :: Either a (Maybe Natural)
-- = 'Right ('Just 5)
--
-- >>> :kind! Eval (Sequence '[ 'Just 3, 'Just 5, 'Just 7])
-- Eval (Sequence '[ 'Just 3, 'Just 5, 'Just 7]) :: Maybe [Natural]
-- = 'Just '[3, 5, 7]
--
-- >>> :kind! Eval (Sequence '[ 'Just 3, 'Nothing, 'Just 7])
-- Eval (Sequence '[ 'Just 3, 'Nothing, 'Just 7]) :: Maybe [Natural]
-- = 'Nothing
--
-- >>> :kind! Eval (Sequence '[ '[1,2], '[3,4]])
-- Eval (Sequence '[ '[1,2], '[3,4]]) :: [[Natural]]
-- = '[ '[1, 3], '[1, 4], '[2, 3], '[2, 4]]
--
--
data SequenceA :: t (f a) -> Exp (f (t a))
type instance Eval (SequenceA tfa) = Eval (Traverse Id tfa)

--------------------------------------------------------------------------------
-- Helper Functions

-- | Helper for [] traverse
data ConsHelper :: (a -> Exp (f b)) -> a -> f [b] -> Exp (f [b])
type instance Eval (ConsHelper f x ys) = Eval (LiftA2 (C.Pure2 '(:)) (Eval (f x)) ys)
-- The following would need an extra import line:
-- type instance Eval (Cons_f f x ys) = Eval (LiftA2 Cons (Eval (f x)) ys)
