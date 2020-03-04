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

'Symbol' also has instances of @('Fcf.Class.Monoid.<>')@ and 'Fcf.Class.Monoid.MEmpty'.

-}
module Fcf.Data.Symbol
    ( -- * Type of symbols
      -- | From "GHC.TypeLits".

      Symbol
    )
  where

import           GHC.TypeLits (Symbol)
