# Revision history for fs-api

## next release -- ????-??-??

### Breaking

* New `primitive ^>=0.9` dependency
* Remove orphan `Show` instance for `Foreign.C.Error.Errno`.
* Provide implementations for the new primitives in the `IO` implementation of
  `HasFS`. As a result, `ioHasFS` now requires that `PrimState IO ~ PrimState m`.
* Rename `Util.CallStack` and `Util.Condense` to `System.FS.CallStack` and
  `System.FS.Condense` respectively.
* Make modules in the `System.FS.IO.Internal` hierarchy public, inspired by
  "Internal convention is a mistake". The following modules are moved/renamed:
  * `System.FS.IO.Internal` is moved to `System.FS.IO.Unix` on Linux and MacOS
    systems, and moved to `System.FS.IO.Windows` on Windows systems.
  * `System.FS.IO.Internal.Handle` is moved to `System.FS.IO.Handle`.

### Non-breaking

* Add new primitives to the `HasFS` interface for performing file I/O with
  user-supplied buffers: `hGetBufSome`, `hGetBufSomeAt`, `hPutBufSome`, and
  `hPutBufSomeAt`.
* Add compound functions, built from the new primitives in `HasFS`:
  `hGetBufExactly`, `hGetBufExactlyAt`, `hPutBufExactly`,  and
  `hPutBufExactlyAt`.
* `NFData` instances for `FsPath`, `HasFS` and `Handle`.
* Add `FsPath` combinators: `(<.>)` and `addExtension`, `(</>)` and `combine`.

### Patch

* Add a clarification in the documentation of `FsPath` that the user is
  responsible for picking sensible directory/file names.
* Bump upper version bounds for `io-classes` to `1.6`
* Make it build with `ghc-9.10`.

## 0.2.0.1 -- 2023-10-30

### Patch

* Make it build with `ghc-9.8`.

## 0.2.0.0 -- 2023-08-01

### Breaking

* Modules that are required for `ioHasFS` should never be used by client code.
  For this reason, we move the relevant modules into an `Internal` hierarchy.
  * Move the `System.IO.FS` module to `System.FS.IO.Internal`.
  * Move the `System.FS.Handle` module to `System.FS.IO.Internal.Handle`.
* Move strict and lazy compound definitions for reading/writing bytes into
  separate modules `System.FS.API.Strict` and `System.FS.API.Lazy`. Both modules
  re-export `System.FS.API`.

### Non-breaking

* Re-export `System.FS.API.Types` from `System.FS.API`.

### Patch

* Bump upper version bounds for `io-classes` to `1.3`

## 0.1.0.3 -- 2023-06-2

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
