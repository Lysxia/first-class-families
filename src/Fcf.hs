{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | First-class type families
--
-- For example, here is a regular type family:
--
-- @
-- type family   FromMaybe (a :: k) (m :: Maybe k) :: k
-- type instance FromMaybe a 'Nothing  = a
-- type instance FromMaybe a ('Just b) = b
-- @
--
-- With @Fcf@, it translates to a @data@ declaration:
--
-- @
-- data FromMaybe :: k -> Maybe k -> 'Exp' k
-- type instance 'Eval' (FromMaybe a 'Nothing)  = a
-- type instance 'Eval' (FromMaybe a ('Just b)) = b
-- @
--
-- - Fcfs can be higher-order.
-- - The kind constructor 'Exp' is a monad: there's @('=<<')@ and 'Pure'.
--
-- Essential language extensions for "Fcf":
--
-- > {-# LANGUAGE
-- >     DataKinds,
-- >     PolyKinds,
-- >     TypeFamilies,
-- >     TypeInType,
-- >     TypeOperators,
-- >     UndecidableInstances #-}

module Fcf
  ( -- * First-class type families

    Exp
  , Eval
  , type (@@)

    -- ** Functional combinators

  , Pure
  , Pure1
  , Pure2
  , Pure3
  , type (=<<)
  , type (<=<)
  , LiftM
  , LiftM2
  , LiftM3
  , Join
  , type (<$>)
  , type (<*>)
  , Flip
  , ConstFn
  , type ($)

    -- * Operations on common types

    -- ** Pairs

  , Uncurry
  , Fst
  , Snd

    -- ** Either

  , UnEither
  , IsLeft
  , IsRight

    -- ** Maybe

  , UnMaybe
  , FromMaybe
  , IsNothing
  , IsJust

    -- ** Lists

  , Foldr
  , UnList
  , type (++)
  , Filter
  , Head
  , Tail
  , Null
  , Length
  , Find
  , FindIndex
  , Lookup
  , SetIndex
  , ZipWith
  , Zip
  , Unzip
  , Cons2

    -- ** Bool

  , UnBool
  , type (||)
  , type (&&)
  , Not

    -- *** Multi-way if

  , Guarded
  , Guard((:=))
  , Otherwise

    -- ** Nat

  , type (+)
  , type (-)
  , type (Fcf.Data.Nat.*)
  , type (<=)
  , type (>=)
  , type (<)
  , type (>)

    -- * Overloaded operations

  , Map
  , Bimap

    -- * Miscellaneous

  , Error
  , Collapse
  , TyEq
  , Stuck
  , IsBool(_If)
  , If

  ) where

import Fcf.Core
import Fcf.Combinators
import Fcf.Data
import Fcf.Data.Bool
import Fcf.Data.Nat
import Fcf.Classes
import Fcf.Utils
