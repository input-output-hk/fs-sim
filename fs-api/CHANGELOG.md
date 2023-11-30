# Revision history for fs-api

## ??? -- ???

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
