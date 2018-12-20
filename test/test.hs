{-# LANGUAGE
    DataKinds,
    KindSignatures,
    TypeOperators #-}

import Data.Type.Equality ((:~:)(Refl))
import GHC.TypeNats (Nat)
import Fcf

type UnitPrefix (n :: Nat) = Eval (Guarded n
  '[ TyEq 0 ':= Pure ""
   , TyEq 1 ':= Pure "deci"
   , TyEq 2 ':= Pure "hecto"
   , TyEq 3 ':= Pure "kilo"
   , TyEq 6 ':= Pure "mega"
   , TyEq 9 ':= Pure "giga"
   ])

-- Compile-time tests

_ = Refl :: UnitPrefix 0 :~: ""
_ = Refl :: UnitPrefix 9 :~: "giga"

-- Dummy

main :: IO ()
main = pure ()
