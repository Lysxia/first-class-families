# 0.8.1.0

- Add `(Fcf.Combinators.>>=)`
- Resolve warnings about deprecated `TypeInType`

# 0.8.0.1

- Bump upper bounds for GHC 9.0
- Update doctests for cabal-docspec

# 0.8.0.0

- Add modules
    + `Fcf.Data.Symbol` (currently just reexports `Symbol`) (thanks to gspia)
    + `Fcf.Data.Function`
    + "Overloaded type families" ("type-level type classes")
        * `Fcf.Class.Ord`
        * `Fcf.Class.Monoid`
        * `Fcf.Class.Monoid.Types` (which exports just an `Endo a` to wrap `a -> Exp a`)
        * `Fcf.Class.Functor`
        * `Fcf.Class.Bifunctor`
        * `Fcf.Class.Foldable`

- Add functions in `Fcf.Data.List`:
  `Intersperse`, `Intercalate`, `Span`, `Break`, `Tails`, `IsPrefixOf`,
  `IsSuffixOf`, `IsInfixOf`, `Partition`.
- Generalize `Foldr`, `Concat` and `ConcatMap` to foldable types.

- Remove deprecated `Guarded`, `Guard((:=))`, `Otherwise`.
- Deprecate `Fcf.Classes`

# 0.7.0.0

- Add `Unfoldr`, `Concat`, `ConcatMap`, `Replicate`, `Take`, `Drop`,
  `TakeWhile`, `DropWhile`, `Reverse` to `Data.List`. (thanks to gspia)
- Change `Elem`, `Lookup`, `Zip` to be `data` instead of `type` synonyms.
- Fix performance of `Filter` and `Find`.

# 0.6.0.0

- Add `Fcf.Utils.Case` (thanks to TheMatten)
- Deprecate `Fcf.Bool.Guarded`
- GHC 8.8 compatibility

# 0.5.0.0

- Modularized library

- `Fcf.Utils`:

    + Add `TError`
    + Rename `Collapse` to `Constraints`

- `Fcf.Data.List`: Added `Cons`, `Last`, `Init`, `Elem`

# 0.4.0.0

- New functions (thanks to blmage)

    + `LiftM`, `LiftM2`, `LiftM3`
    + `(<=)`, `(>=)`, `(<)`, `(>)`
    + `Guarded`, `Guard((:=))`, `Otherwise`

# 0.3.0.1

- GHC 8.6 compatibility

# 0.3.0.0

- More new functions, (thanks to isovector)

# 0.2.0.0

- A whole bunch of basic functions (thanks to isovector)
- Remove `Traverse` (now `Map`), `BimapPair`, `BimapEither` (now `Bimap`)

# 0.1.0.0

Initial version
