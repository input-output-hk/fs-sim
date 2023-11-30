# Revision history for fs-sim

## ??? -- ???

### Patch

* Make it build with `ghc-9.8`.

## 0.2.1.0 -- 2023-08-01

### Non-breaking

* Build with `fs-api ^>=0.2`.

### Patch

* Bump upper version bounds for `io-classes` and `strict-stm` to `1.3`

## 0.2.0.0 -- 2023-06-02

### Breaking

* Move `Stream`-related functions to new `System.FS.Sim.Stream` module.
* Remove `Semigroup` and `Monoid` instances for `Stream` and `Errors` types.
* Overhaul `Stream` type and related functions. The `Stream` type now behaves
  similarly to `Test.QuickCheck`'s `InfiniteList`, which improves showing,
  generation and shrinking.

### Non-breaking

* Add `simHasFS'` and `mkSimErrorHasFS'`, which are alternatives to `simHasFS`
  and `mkSimErrorHasFS` that create `TVar`s internally.
* Add new `emptyErrors` function.
* Adapt the `Errors` type to use the overhauled `Stream` type. As a bonus:
  * Arbitrary `Errors` now contain infinite error `Stream`s by default, instead
    of finite ones.
  * Shrinking of `Errors` that contain infinite error `Stream`s now terminates.

### Patch

* Enable building with `ghc-9.4`.
* Remove `asserts` package flag.

## 0.1.0.2 -- 2023-05-25

* Enable building with ghc-9.6

## 0.1.0.1 -- 2023-04-24

### Non-breaking

Update the code to compile with `io-sim-1.0.0.1`.

## 0.1.0.0 -- 2023-03-27

* First version. Released on an unsuspecting world.
