# First-class type families [![Hackage](https://img.shields.io/hackage/v/first-class-families.svg)](https://hackage.haskell.org/package/first-class-families) [![Build Status](https://travis-ci.org/Lysxia/first-class-families.svg)](https://travis-ci.org/Lysxia/first-class-families)

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

Please, note that `FromMaybe 0` is partially applied type-level function.

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

## See also

[Haskell with only one type family](http://blog.poisson.chat/posts/2018-08-06-one-type-family.html) (blogpost)

---

Contributions are welcome. Feel free to open an issue or make a PR on
[Github](https://github.com/Lysxia/first-class-families)!
