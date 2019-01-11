{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}

module Fcf.Data.Bool
  ( UnBool
  , type (||)
  , type (&&)
  , Not

    -- *** Multi-way if

  , Guarded
  , Guard((:=))
  , Otherwise
  ) where

import Fcf.Core
import Fcf.Combinators (ConstFn)
import Fcf.Utils

-- | N.B.: The order of the two branches is the opposite of "if":
-- @UnBool ifFalse ifTrue bool@.
--
-- This mirrors the default order of constructors:
--
-- @
-- data Bool = False | True
-- ----------- False < True
-- @
data UnBool :: Exp a -> Exp a -> Bool -> Exp a
type instance Eval (UnBool fal tru 'False) = Eval fal
type instance Eval (UnBool fal tru 'True ) = Eval tru

infixr 2 ||
infixr 3 &&

data (||) :: Bool -> Bool -> Exp Bool
type instance Eval ('True || b) = 'True
type instance Eval (a || 'True) = 'True
type instance Eval ('False || b) = b
type instance Eval (a || 'False) = a

data (&&) :: Bool -> Bool -> Exp Bool
type instance Eval ('False && b) = 'False
type instance Eval (a && 'False) = 'False
type instance Eval ('True && b) = b
type instance Eval (a && 'True) = a

data Not :: Bool -> Exp Bool
type instance Eval (Not 'True)  = 'False
type instance Eval (Not 'False) = 'True

-- | A conditional choosing the first branch whose guard @a -> 'Exp' 'Bool'@
-- accepts a given value @a@.
--
-- === Example
--
-- @
-- type UnitPrefix n = 'Eval' ('Guarded' n
--   '[ 'TyEq' 0 \'':=' 'Pure' \"\"
--    , 'TyEq' 1 \'':=' 'Pure' \"deci\"
--    , 'TyEq' 2 \'':=' 'Pure' \"hecto\"
--    , 'TyEq' 3 \'':=' 'Pure' \"kilo\"
--    , 'TyEq' 6 \'':=' 'Pure' \"mega\"
--    , 'TyEq' 9 \'':=' 'Pure' \"giga\"
--    , 'Otherwise' \'':=' 'Error' "Something else"
--    ])
-- @
data Guarded :: a -> [Guard (a -> Exp Bool) (Exp b)] -> Exp b
type instance Eval (Guarded x ((p ':= y) ': ys)) =
    Eval (If (Eval (p x)) y (Guarded x ys))

-- | A fancy-looking pair type to use with 'Guarded'.
data Guard a b = a := b
infixr 0 :=

-- | A catch-all guard for 'Guarded'.
type Otherwise = ConstFn 'True

