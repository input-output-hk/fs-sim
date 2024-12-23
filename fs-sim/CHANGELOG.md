# Revision history for fs-sim

## ?.?.?.? -- ????-??-??

### Breaking

* Fix a bug where `withErrors` would not put back the previous `Errors` when an
  exception is thrown during execution of the function. Though we fixed the bug,
  it is also a breaking change: the type signature now has an additional
  constraint.

## 0.3.1.0 -- 2024-12-10

### Non-breaking

* Expose `openHandles` for testing.

### Patch

* Make `genInfinite` generate truly infinite streams.
* The shrinker for `Errors` now truly shrinks towards empty errors.

## 0.3.0.1 -- 2024-10-02

### Patch

* Support `io-classes-1.6` and `io-classes-1.7`. In these versions, `strict-stm`
  has become a public sub-library of `io-classes`. As a result, older versions
  of `io-classes` are no longer supported, and we now depend on
  `io-classes:strict-stm` instead of `strict-stm`.

## 0.3.0.0 -- 2024-08-26

### Breaking

* Orphan `Show` instance for `Foreign.C.Error.Errno` removed by `fs-api`.
* Rename some functions related to partial reads/writes and corruption in `System.FS.Sim.Error`:
  * Replace `hGetSomePartial` by `partialiseByteCount`/`partialiseWord64`.
  * Replace `hPutSomePartial` by `partialiseByteString`
  * Replace `corrupt` by `corruptByteString`
* Remove `System.FS.Sim.Pure` module.
* Adapt `simHasFS` to the new `HasFS` primitives. This leads to two breaking
  changes:
  * Add a `PrimMonad m` constraint to `runSimFS`, `simHasFS'` and `simHasFS`.
  * Change the `StrictTVar` argument to `simHasFS` to a `StrictTMVar`.
* Adapt `mkSimErrorHasFS` to the new `HasFS` primitives. This leads to two
  breaking changes:
  * Add a `PrimMonad m` constraint to `runSimErrorFS`, `mkSimErrorHasFS'` and `mkSimErrorHasFS`.
  * Change the `StrictTVar` argument to `mkSimErrorHasFS` to a `StrictTMVar`.
* Rename `mkSimErrorHasFS` to `simErrorHasFS`.
* Rename `mkSimErrorHasFS'` to `simErrorHasFS'`.

### Non-breaking

* New constructors for the `Errors` type: `hGetBufSomeE`, `hGetBufSomeAtE`,
  `hGetBufSomeE`, and `hPutBufSomeAtE`.
* Expose the new `System.FS.Sim.Prim` module.

### Patch

* `allNull` was not actually checking whether all streams in the argument
  `Errors` are empty.
* The `Show Errors` instance was not printing every stream.
* The shrinker for `Errors` was not shrinking every stream.
* Adapt to moving of `Util` modules in `fs-api`.
* Make it build with `ghc-9.10`.
* New `primitive ^>=0.9` dependency
* New `safe-wild-cards^>=1.0`dependency
* Tight dependency bounds.

## 0.2.1.1 -- 2023-10-30

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
