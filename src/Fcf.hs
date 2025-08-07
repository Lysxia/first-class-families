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
  , Pure4
  , Pure5
  , Pure6
  , Pure7
  , Pure8
  , Pure9
  , type (=<<)
  , type (>>=)
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

  , type (++)
  , Head
  , Last
  , Tail
  , Cons
  , Snoc
  , Cons2
  , Init
  , Uncons
  , Unsnoc
  , Singleton
  , Null
  , Length
  , Reverse
  , Intersperse
  , Intercalate
  , Foldr
  , UnList
  , Concat
  , ConcatMap
  , Unfoldr
  , Replicate
  , Take
  , Drop
  , SplitAt
  , TakeWhile
  , DropWhile
  , Span
  , Break
  , Tails
  , IsPrefixOf
  , IsSuffixOf
  , IsInfixOf
  , Elem
  , Lookup
  , Find
  , Filter
  , Partition
  , FindIndex
  , SetIndex
  , ZipWith
  , Zip
  , Unzip

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
