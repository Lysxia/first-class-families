{-# LANGUAGE
    DataKinds,
    KindSignatures,
    TypeOperators #-}

import Data.Type.Equality ((:~:)(Refl))
import Fcf

type UnitPrefix = Case
  [ 0 --> ""
  , 1 --> "deci"
  , 2 --> "hecto"
  , 3 --> "kilo"
  , 6 --> "mega"
  , 9 --> "giga"
  , Any   (Error @@ "Something Else")
  ]

-- Compile-time tests

_ = Refl :: Eval (UnitPrefix 0) :~: ""
_ = Refl :: Eval (UnitPrefix 3) :~: "kilo"

-- Dummy

main :: IO ()
main = pure ()
