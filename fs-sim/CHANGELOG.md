# Revision history for fs-sim

## next version

### Breaking

* Move `Stream`-related functions to new `System.FS.Sim.Stream` module.

### Non-breaking

* Add `simHasFS'` and `mkSimErrorHasFS'`, which are alternatives to `simHasFS`
  and `mkSimErrorHasFS` that create `TVar`s internally.

### Patch

* Enable building with `ghc-9.4`.

## 0.1.0.2 -- 2023-05-25

* Enable building with ghc-9.6

## 0.1.0.1 -- 2023-04-24

### Non-breaking

Update the code to compile with `io-sim-1.0.0.1`.

## 0.1.0.0 -- 2023-03-27

* First version. Released on an unsuspecting world.
