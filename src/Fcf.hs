{-# LANGUAGE TypeOperators #-}

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

  , type (.)
  , Flip
  , ConstFn
  , type ($)

    -- * Operations on common types

    -- ** Pairs

  , Uncurry
  , Fst
  , Snd
  , type (***)

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

    -- ** Case splitting

  , Case
  , Match()
  , type (-->)
  , Is
  , Any
  , Else

    -- ** Nat

  , type (+)
  , type (-)
  , type (Fcf.Data.Nat.*)
  , type (^)
  , type (<=)
  , type (>=)
  , type (<)
  , type (>)

    -- * Overloaded operations

  , Map
  , Bimap

    -- * Miscellaneous

  , Error
  , Constraints
  , TyEq
  , Stuck
  , IsBool(_If)
  , If

  ) where

import Fcf.Core
import Fcf.Combinators
import Fcf.Data.Bool
import Fcf.Data.Common
import Fcf.Data.List
import Fcf.Data.Nat
import Fcf.Class.Functor
import Fcf.Class.Bifunctor
import Fcf.Utils
