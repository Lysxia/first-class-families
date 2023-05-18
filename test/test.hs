{-# LANGUAGE
    CPP,
    DataKinds,
    KindSignatures,
    TypeOperators #-}

import Data.Type.Equality ((:~:)(Refl))
import qualified Data.Monoid as Monoid

import Fcf.Core (Eval, type (@@))
import Fcf.Combinators
import Fcf.Utils (Case, type (-->), Error)
import qualified Fcf.Utils as Case

import Fcf.Class.Bifunctor
import Fcf.Class.Foldable
import Fcf.Class.Monoid
import Fcf.Class.Ord
import Fcf.Class.Eq

import Fcf.Data.Function
import Fcf.Data.List
import Fcf.Data.Nat (type (+))

type UnitPrefix = Case
  [ 0 --> ""
  , 1 --> "deci"
  , 2 --> "hecto"
  , 3 --> "kilo"
  , 6 --> "mega"
  , 9 --> "giga"
  , Case.Any   (Error @@ "Something Else")
  ]

-- Compile-time tests

_ = Refl :: Eval (UnitPrefix 0) :~: ""
_ = Refl :: Eval (UnitPrefix 3) :~: "kilo"

-- * Class

-- ** Eq

_ = Refl :: ('Left 5 == 'Right 5) :~: 'False
_ = Refl :: ('Right 5 == 'Right 5) :~: 'True
_ = Refl :: (Nothing == 'Just 5) :~: 'False
_ = Refl :: ('Just "h" == 'Just "H") :~: 'False
_ = Refl :: ('[ '( '(), 'LT) ] == '[ '( '(), 'LT) ]) :~: 'True

-- ** Ord

_ = Refl :: Compare '( '(), 0 ) '( '(), 1 ) :~: 'LT
_ = Refl :: Compare '( 1, 3 ) '( 1, 2 ) :~: 'GT
_ = Refl :: Compare ('Left '()) ('Right 'LT) :~: 'LT
_ = Refl :: Compare ('Right 'EQ) ('Right 'EQ) :~: 'EQ
_ = Refl :: Compare '[ 'LT, 'EQ, 'GT ] '[ 'LT, 'EQ, 'GT ] :~: 'EQ
_ = Refl :: Compare 'True 'True :~: 'EQ
_ = Refl :: Compare "A" "B" :~: 'LT

_ = Refl :: 1 <= 1 :~: 'True
_ = Refl :: 2 <= 1 :~: 'False
_ = Refl :: 1 < 1 :~: 'False
_ = Refl :: 1 < 2 :~: 'True
_ = Refl :: 1 >= 1 :~: 'True
_ = Refl :: 1 >= 2 :~: 'False
_ = Refl :: 1 > 1 :~: 'False
_ = Refl :: 2 > 1 :~: 'True

-- ** Monoid

_ = Refl :: Eval ('( '(), '[ 'LT, 'EQ ]) .<> '( '(), '[ 'GT ])) :~: '( '(), '[ 'LT, 'EQ, 'GT ])
_ = Refl :: Eval ('Nothing .<> 'Just '[]) :~: 'Just '[]
_ = Refl :: Eval ('LT .<> 'GT) :~: 'LT
_ = Refl :: Eval ('EQ .<> 'GT) :~: 'GT
_ = Refl :: Eval ('Monoid.All 'True .<> 'Monoid.All 'False) :~: 'Monoid.All 'False
_ = Refl :: Eval ('Monoid.Any 'True .<> 'Monoid.Any 'False) :~: 'Monoid.Any 'True
#if __GLASGOW_HASKELL__ >= 802
_ = Refl :: Eval ("a" .<> MEmpty) :~: "a"
#endif

-- ** Foldable

_ = Refl :: Eval (FoldMap (Pure1 'Monoid.All) '[ 'True, 'False ]) :~: 'Monoid.All 'False
_ = Refl :: Eval (FoldMap (Pure1 'Monoid.All) 'Nothing) :~: 'Monoid.All 'True
_ = Refl :: Eval (Foldr (.<>) 'LT '[ 'EQ, 'EQ ]) :~: 'LT
_ = Refl :: Eval (And '[ 'False, 'False ]) :~: 'False
_ = Refl :: Eval (Or '[ 'False, 'False ]) :~: 'False
_ = Refl :: Eval (Concat ('Right 'LT)) :~: 'LT

_ = Refl :: FoldMapDefault_ (Pure1 'Monoid.All) 'Nothing :~: 'Monoid.All 'True
_ = Refl :: FoldrDefault_ (.<>) 'LT '[ 'EQ, 'EQ ] :~: 'LT

-- ** Functor

_ = Refl :: Eval (Bimap ((+) 1) (Pure2 '(:) '()) '(8, '[])) :~: '(9, '[ '()])
_ = Refl :: Eval (First ((+) 1) ('Left 8)) :~: 'Left 9
_ = Refl :: Eval (First ((+) 1) ('Right 0)) :~: 'Right 0
_ = Refl :: Eval (Second ((+) 1) ('Left 0)) :~: 'Left 0
_ = Refl :: Eval (Second ((+) 1) ('Right 8)) :~: 'Right 9

-- ** Function

_ = Refl :: Eval (3 & Pure) :~: 3
_ = Refl :: Eval (((+) `On` Length) '[1,2,3] '[1,2]) :~: 5

-- Dummy

main :: IO ()
main = pure ()
