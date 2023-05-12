{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    StandaloneKindSignatures,
    UndecidableInstances #-}

-- | Equality class, internal
--
-- Contains both Exp and normal versions of functions
--
module Fcf.Class.Eq.Internal where

import Data.Type.Bool
import qualified GHC.TypeLits as TL
import Fcf.Utils
import Fcf.Core

infix 4 ==, /=

type (==) :: a -> a -> Bool
type family (==) x y

type (/=) a b = Not (a == b)

data AndExp :: a -> a -> Exp Bool
type instance Eval (AndExp a b) = a && b

data NotExp :: Bool -> Exp Bool
type instance Eval (NotExp a) = Not a

data OrExp :: a -> a -> Exp Bool
type instance Eval (OrExp a b) = a || b

data NotEqExp :: a -> a -> Exp Bool
type instance Eval (NotEqExp a b) = a /= b

data EqExp :: a -> a -> Exp Bool
type instance Eval (EqExp a b) = a == b

-- (,)
type instance (==) '(a1, a2) '(b1, b2) = a1 == b1 && a2 == b2

-- (,,)
type instance (==) '(a1, a2, a3) '(b1, b2, b3) = a1 == b1 && a2 == b2 && a3 == b3

-- (,,,)
type instance (==) '(a1, a2, a3, a4) '(b1, b2, b3, b4) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4

-- (,,,,)
type instance (==) '(a1, a2, a3, a4, a5) '(b1, b2, b3, b4, b5) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5

-- List
type instance (==) '[] '[] = 'True
type instance (==) (x ': xs) (y ': ys) = x == y && xs == ys
type instance (==) (_ ': _) '[] = 'False
type instance (==) '[] (_ ': _) = 'False

-- Maybe
type instance (==) 'Nothing 'Nothing = 'True
type instance (==) ('Just _) 'Nothing = 'False 
type instance (==) 'Nothing ('Just _) = 'False 
type instance (==) ('Just x) ('Just y) = x == y

-- Either
type instance (==) ('Left x) ('Left y) = x == y
type instance (==) ('Right x) ('Right y) = x == y
type instance (==) ('Left _) ('Right y) = 'False
type instance (==) ('Right _) ('Left _) = 'False

-- ()
type instance (==) '() '() = 'True

-- Ordering
type instance (==) 'LT 'EQ = 'False
type instance (==) 'LT 'GT = 'False
type instance (==) 'LT 'LT = 'True
type instance (==) 'EQ 'GT = 'False
type instance (==) 'EQ 'LT = 'False
type instance (==) 'EQ 'EQ = 'True
type instance (==) 'GT 'LT = 'False
type instance (==) 'GT 'EQ = 'False
type instance (==) 'GT 'GT = 'True

-- Symbol
type instance (==) x y = Eval (Case '[ 'LT --> 'False
                                     , 'GT --> 'False
                                     , 'EQ --> 'True
                                     ] (TL.CmpSymbol x y))

-- Nat
type instance (==) x y = Eval (Case '[ 'LT --> 'False
                                     , 'GT --> 'False
                                     , 'EQ --> 'True
                                     ] (TL.CmpNat x y))
