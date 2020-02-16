{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Data.Symbol
Description : Type-level strings

= Symbols

Type-level strings.

Note that the operators from this module conflict with "GHC.TypeLits".

-}
module Fcf.Data.Symbol
    ( -- * Reexported type
      -- | From "GHC.TypeLits".

      Symbol

     -- * Comparison functions
    , type (<=)
    , type (>=)
    , type (<)
    , type (>)
    , type OrdEq
    )
  where

import           GHC.TypeLits (Symbol)
import qualified GHC.TypeLits as TL

import           Fcf.Core (Eval, Exp)
import           Fcf.Classes (Compare)
import           Fcf.Combinators (type (=<<))
import           Fcf.Data.Bool (Not)
import           Fcf.Utils (TyEq)


-- | 'Compare' instance for 'Symbols'.
--
-- === __Example__
--
-- >>> :kind! Eval (Compare "a" "b")
-- Eval (Compare "a" "b") :: Ordering
-- = 'LT
type instance Eval (Compare a b) = TL.CmpSymbol a b

-- | Less-than-or-equal comparison for symbols.
--
-- === __Example__
--
-- >>> :kind! Eval ("b" <= "a")
-- Eval ("b" <= "a") :: Bool
-- = 'False
data (<=) :: Symbol -> Symbol -> Exp Bool
type instance Eval ((<=) a b) = IsLE (Eval (Compare a b))

type family IsLE (o :: Ordering) :: Bool where
    IsLE 'LT = 'True
    IsLE 'EQ = 'True
    IsLE _   = 'False

-- | Larger-than-or-equal comparison for symbols.
--
-- === __Example__
--
-- >>> :kind! Eval ("b" >= "a")
-- Eval ("b" >= "a") :: Bool
-- = 'True
data (>=) :: Symbol -> Symbol -> Exp Bool
type instance Eval ((>=) a b) =  Eval (Not =<< TyEq (Eval (Compare a b)) 'LT)


-- | Less-than comparison for symbols.
--
-- === __Example__
--
-- >>> :kind! Eval ("a" < "b")
-- Eval ("a" < "b") :: Bool
-- = 'True
data (<) :: Symbol -> Symbol -> Exp Bool
type instance Eval ((<) a b) = Eval (TyEq (TL.CmpSymbol a b) 'LT)

-- | Larger-than comparison for symbols.
--
-- === __Example__
--
-- >>> :kind! Eval ("b" > "a")
-- Eval ("b" > "a") :: Bool
-- = 'True
data (>) :: Symbol -> Symbol -> Exp Bool
type instance Eval ((>) a b) = Eval (TyEq (TL.CmpSymbol a b) 'GT)

-- | Equality of symbols.
--
-- === __Example__
--
-- >>> :kind! Eval (OrdEq "b" "a")
-- Eval (OrdEq "b" "a") :: Bool
-- = 'False
data OrdEq :: Symbol -> Symbol -> Exp Bool
type instance Eval (OrdEq a b) = Eval (TyEq (Eval (Compare a b)) 'EQ)
