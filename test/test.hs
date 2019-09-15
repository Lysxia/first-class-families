{-# LANGUAGE
    DataKinds,
    KindSignatures,
    TypeOperators #-}

import Data.Type.Equality ((:~:)(Refl))
import GHC.TypeLits (Nat)
import Fcf

type UnitPrefix (n :: Nat) = Eval (Guarded n
  [ TyEq 0 ':= Pure ""
  , TyEq 1 ':= Pure "deci"
  , TyEq 2 ':= Pure "hecto"
  , TyEq 3 ':= Pure "kilo"
  , TyEq 6 ':= Pure "mega"
  , TyEq 9 ':= Pure "giga"
  , Otherwise ':= Error "Something else"
  ])

type UnitPrefix' = Case
  [ 0 --> ""
  , 1 --> "deci"
  , 2 --> "hecto"
  , 3 --> "kilo"
  , 6 --> "mega"
  , 9 --> "giga"
  , Any   (Error @@ "Something Else")
  ]

-- Compile-time tests

_ = Refl :: UnitPrefix 0 :~: ""
_ = Refl :: UnitPrefix 9 :~: "giga"

_ = Refl :: Eval (UnitPrefix' 0) :~: ""
_ = Refl :: Eval (UnitPrefix' 3) :~: "kilo"

-- Dummy

main :: IO ()
main = pure ()
