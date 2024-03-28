{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeOperators #-}

-- | The 'Eval' family.

module Fcf.Core
  ( Exp
  , Eval
  , type (@@)
  ) where

import Data.Kind (Type)

-- * First-class type families

-- | Kind of type-level expressions indexed by their result type.
type Exp a = a -> Type

-- | Expression evaluator.
type family Eval (e :: Exp a) :: a

-- ** Miscellaneous

-- | Apply and evaluate a unary type function.
type f @@ x = Eval (f x)

