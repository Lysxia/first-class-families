# First-class type families [![Hackage](https://img.shields.io/hackage/v/first-class-families.svg)](https://hackage.haskell.org/package/first-class-families) [![Build Status](https://travis-ci.org/Lysxia/first-class-families.svg)](https://travis-ci.org/Lysxia/first-class-families)

First-class type families are type-level functions that can be
composed using higher-order functions.

The core of the idea is an extensible kind of "type-level expressions"
and an open type family for evaluating such expressions.

```haskell
type Exp (k :: Type) :: Type
type family Eval (e :: Exp k) :: k
```

This library provides that core foundation,
and also exports basic first-class type families.

## Example

For example, consider this simple type family:

```haskell
type family   FromMaybe (a :: k) (m :: Maybe k) :: k
type instance FromMaybe a 'Nothing  = a
type instance FromMaybe a ('Just b) = b
```

With first-class-families (fcfs), it translates to a `data` declaration
and instances for a single `Eval` family:

```haskell
import Fcf

data FromMaybe :: k -> Maybe k -> Exp k
type instance Eval (FromMaybe a 'Nothing)  = a
type instance Eval (FromMaybe a ('Just b)) = b
```

That way, the `FromMaybe` constructor can be partially applied,
and passed to higher-order fcfs such as `Map`:

```haskell
Eval (Map (FromMaybe 0) '[ 'Just 1, 'Nothing ])  =  '[ 1, 0 ] :: [Nat]
```

Essential language extensions:

```haskell
{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    UndecidableInstances #-}
```

## Overview

- `Fcf.Core`: definition of `Exp` and `Eval`.
- `Fcf.Combinators`: general combinators to compose first-class families.
- `Fcf.Data.*`: first-class families on common data types.
- `Fcf.Class.*`: overloaded first-class families.
- `Fcf.Utils`: miscellaneous.

The top-level module `Fcf` is a prelude to get acquainted with the library.
For regular use, import what you need from the specialized modules
above, preferably with explicit import lists.

```haskell
import Fcf                       -- Simple but fragile

import Fcf.Class.Functor (FMap)  -- Explicit and robust
```

## Features

### Overloaded type families

Value-level functions can be overloaded using type classes.
Type families---type-level functions---are open by design,
so overloading is as easy as just declaring them with more general types.

```haskell
data Map :: (a -> Exp b) -> f a -> Exp (f b)

-- Instances for f = []
type instance Eval (Map f '[]) = '[]
type instance Eval (Map f (x ': xs)) = Eval (f x) ': Eval (Map f xs)

-- Instances for f = Maybe
type instance Eval (Map f 'Nothing) = 'Nothing
type instance Eval (Map f ('Just x)) = 'Just (Eval (f x))
```

## See also

- [Haskell with only one type family](http://blog.poisson.chat/posts/2018-08-06-one-type-family.html)
- [Overloaded type families](https://blog.poisson.chat/posts/2018-09-29-overloaded-families.html)
- [The *singletons* library](https://hackage.haskell.org/package/singletons)

---

Contributions are welcome. Feel free to open an issue or make a PR on
[Github](https://github.com/Lysxia/first-class-families)!
