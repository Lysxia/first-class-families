{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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

module Fcf where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol, Nat, type (+), TypeError, ErrorMessage(..))

-- * First-class type families

-- | Kind of type-level expressions indexed by their result type.
type Exp a = a -> Type

-- | Expression evaluator.
type family Eval (e :: Exp a) :: a

-- ** Monadic operations

infixr 1 =<<, <=<
infixl 4 <$>, <*>

data Pure :: a -> Exp a
type instance Eval (Pure x) = x

data Pure1 :: (a -> b) -> a -> Exp b
type instance Eval (Pure1 f x) = f x

data Pure2 :: (a -> b -> c) -> a -> b -> Exp c
type instance Eval (Pure2 f x y) = f x y

data Pure3 :: (a -> b -> c -> d) -> a -> b -> c -> Exp d
type instance Eval (Pure3 f x y z) = f x y z

data (=<<) :: (a -> Exp b) -> Exp a -> Exp b
type instance Eval (k =<< e) = Eval (k (Eval e))

data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c
type instance Eval ((f <=< g) x) = Eval (f (Eval (g x)))

data Join :: Exp (Exp a) -> Exp a
type instance Eval (Join e) = Eval (Eval e)

data (<$>) :: (a -> b) -> Exp a -> Exp b
type instance Eval (f <$> e) = f (Eval e)

data (<*>) :: Exp (a -> b) -> Exp a -> Exp b
type instance Eval (f <*> e) = Eval f (Eval e)

-- ** More higher-order combinators

data Flip :: (a -> b -> Exp c) -> b -> a -> Exp c
type instance Eval (Flip f y x) = Eval (f x y)

data Uncurry :: (a -> b -> Exp c) -> (a, b) -> Exp c
type instance Eval (Uncurry f '(x, y)) = Eval (f x y)

data BimapPair :: (a -> Exp a') -> (b -> Exp b') -> (a, b) -> Exp (a', b')
type instance Eval (BimapPair f g '(x, y)) = '(Eval (f x), Eval (g y))

data UnEither :: (a -> Exp c) -> (b -> Exp c) -> Either a b -> Exp c
type instance Eval (UnEither f g ('Left  x)) = Eval (f x)
type instance Eval (UnEither f g ('Right y)) = Eval (g y)

data BimapEither
  :: (a -> Exp a') -> (b -> Exp b') -> Either a b -> Exp (Either a' b')
type instance Eval (BimapEither f g ('Left  x)) = 'Left  (Eval (f x))
type instance Eval (BimapEither f g ('Right y)) = 'Right (Eval (g y))

data UnMaybe :: Exp b -> (a -> Exp b) -> Maybe a -> Exp b
type instance Eval (UnMaybe y f 'Nothing) = Eval y
type instance Eval (UnMaybe y f ('Just x)) = Eval (f x)

data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance Eval (Foldr f y '[]) = y
type instance Eval (Foldr f y (x ': xs)) = Eval (f x (Eval (Foldr f y xs)))

data Traverse :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval (Traverse f '[]) = '[]
type instance Eval (Traverse f (x ': xs)) = Eval (f x) ': Eval (Traverse f xs)

-- | N.B.: This is equivalent to a 'Foldr' flipped.
data UnList :: b -> (a -> b -> Exp b) -> [a] -> Exp b
type instance Eval (UnList y f xs) = Eval (Foldr f y xs)

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

-- ** Primitives

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

-- * Helpful shorthands

-- | Apply and evaluate a unary type function.
type f @@ x = Eval (f x)

-- * Reification

class IsBool (b :: Bool) where
  _If :: ((b ~ 'True) => r) -> ((b ~ 'False) => r) -> r

instance IsBool 'True  where _If a _ = a
instance IsBool 'False where _If _ b = b

type family   If (b :: Bool) (x :: k) (y :: k) :: k
type instance If 'True   x _y = x
type instance If 'False _x  y = y


data Error :: Symbol -> Exp a
type instance Eval (Error msg) = TypeError ('Text msg)

data ConstFn :: a -> b -> Exp a
type instance Eval (ConstFn a _b) = a

data (++) :: [a] -> [a] -> Exp [a]
type instance Eval ((++) '[] ys) = ys
type instance Eval ((++) (x ': xs) ys) = x ': Eval ((++) xs ys)

data Filter :: (a -> Exp Bool) -> [a] -> Exp [a]
type instance Eval (Filter _p '[]) = '[]
type instance Eval (Filter p (a ': as)) =
  If (Eval (p a))
    (a ': Eval (Filter p as))
    (Eval (Filter p as))


data Head :: [a] -> Exp (Maybe a)
type instance Eval (Head '[]) = 'Nothing
type instance Eval (Head (a ': _as)) = 'Just a

data Tail :: [a] -> Exp (Maybe [a])
type instance Eval (Tail '[]) = 'Nothing
type instance Eval (Tail (_a ': as)) = 'Just as

data Null :: [a] -> Exp Bool
type instance Eval (Null '[]) = 'True
type instance Eval (Null (a ': as)) = 'False

data Length :: [a] -> Exp Nat
type instance Eval (Length '[]) = 0
type instance Eval (Length (a ': as)) = 1 + Eval (Length as)

data Collapse :: [Constraint] -> Exp Constraint
type instance Eval (Collapse '[]) = () ~ ()
type instance Eval (Collapse (a ': as)) = (a, Eval (Collapse as))

data Fst :: (a, b) -> Exp a
type instance Eval (Fst '(a, _b)) = a

data Snd :: (a, b) -> Exp b
type instance Eval (Snd '(_a, b)) = b

data TyEq :: a -> b -> Exp Bool
type instance Eval (TyEq a b) = TyEqImpl a b

type family TyEqImpl (a :: k) (b :: k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl a b = 'False

infixr 0 $
data ($) :: (a -> Exp b) -> a -> Exp b
type instance Eval (($) f a) = Eval (f a)

data Find :: (a -> Exp Bool) -> [a] -> Exp (Maybe a)
type instance Eval (Find _p '[]) = 'Nothing
type instance Eval (Find p (a ': as)) =
  If (Eval (p a))
    ('Just a)
    (Eval (Find p as))

type Lookup (a :: k) (as :: [(k, b)]) =
  Map Snd (Eval (Find (($) (TyEq a) <=< Fst) as))

data ZipWith :: (a -> b -> Exp c) -> [a] -> [b] -> Exp [c]
type instance Eval (ZipWith _f '[] _bs) = '[]
type instance Eval (ZipWith _f _as '[]) = '[]
type instance Eval (ZipWith f (a ': as) (b ': bs)) =
  Eval (f a b) ': Eval (ZipWith f as bs)

type Zip = ZipWith (Pure2 '(,))

data Unzip :: Exp [(a, b)] -> Exp ([a], [b])
type instance Eval (Unzip as) = Eval (Foldr Cons2 '( '[], '[]) (Eval as))

data Cons2 :: (a, b) -> ([a], [b]) -> Exp ([a], [b])
type instance Eval (Cons2 '(a, b) '(as, bs)) = '(a ': as, b ': bs)

infixr 3 ***
data (***) :: (b -> Exp c) -> (b' -> Exp c') -> (b, b') -> Exp (c, c')
type instance Eval ((***) f f' '(b, b')) = '(Eval (f b), Eval (f' b'))


data Map :: (a -> Exp b) -> f a -> Exp (f b)
type instance Eval (Map f '[]) = '[]
type instance Eval (Map f (a ': as)) = Eval (f a) ': Eval (Map f as)

type instance Eval (Map f 'Nothing) = 'Nothing
type instance Eval (Map f ('Just a)) = 'Just (Eval (f a))

type instance Eval (Map f ('Left x)) = 'Left x
type instance Eval (Map f ('Right a)) = 'Right (Eval (f a))

type instance Eval (Map f '(x, a)) =
  '(x, Eval (f a))
type instance Eval (Map f '(x, y, a)) =
  '(x, y, Eval (f a))
type instance Eval (Map f '(x, y, z, a)) =
  '(x, y, z, Eval (f a))
type instance Eval (Map f '(x, y, z, w, a)) =
  '(x, y, z, w, Eval (f a))

