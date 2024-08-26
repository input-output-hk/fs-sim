# fs-sim

[![handbook](https://img.shields.io/badge/policy-Cardano%20Engineering%20Handbook-informational)](https://input-output-hk.github.io/cardano-engineering-handbook)
[![Hackage Version](https://img.shields.io/hackage/v/fs-sim?label=hackage%20fs-sim)](https://hackage.haskell.org/package/fs-sim)
[![Hackage Version](https://img.shields.io/hackage/v/fs-api?label=hackage%20fs-api)](https://hackage.haskell.org/package/fs-api)
[![Haskell CI](https://img.shields.io/github/actions/workflow/status/input-output-hk/fs-sim/haskell.yml?label=Build)](https://github.com/input-output-hk/fs-sim/actions/workflows/haskell.yml)
[![Documentation CI](https://img.shields.io/github/actions/workflow/status/input-output-hk/fs-sim/documentation.yml?label=Documentation%20build)](https://github.com/input-output-hk/fs-sim/actions/workflows/documentation.yml)
[![Haddocks](https://img.shields.io/badge/documentation-Haddocks%20(dev%20version)-purple)](https://input-output-hk.github.io/fs-sim/)


The [`fs-sim`](./fs-sim/README.md) package provides a filesystem simulator that
facilitates simulation of errors and file corruption. This simulator is
specially useful for testing purposes, and works well in conjunction with
[`io-sim`](ps://github.com/input-output-hk/io-sim).

The [`fs-api`](./fs-api/README.md) package provides an abstract interface to
filesystems. Code that is written using this interface can be run against the
built-in file system (required to be in `IO`), a simulated file system as
provided by `fs-sim`, or any other implementation of a file system.

## Note on GHC compatibility

`fs-sim` and `fs-api` are unlikely to work with GHC versions before
`ghc-8.10.7`. See the `tested-with` fields in
[`fs-api.cabal`](./fs-api/fs-api.cabal) and
[`fs-sim.cabal`](./fs-sim/fs-sim.cabal) for the GHC versions that we test with.

## Credits

Previously, the packages in this repository lived in the [`ouroboros-network`
repository](https://github.com/input-output-hk/ouroboros-network). [This
commit](https://github.com/input-output-hk/ouroboros-network/commit/0659e7b7ef66b01763cdf15c2f8777a9394c248d)
marks the migration of the packages from `ouroboros-network` to the current
repository. Since the commit history was not carried over to this repository, we
give credit for the packages to the original authors and frequent contributors, which include:

* Thomas Winant (@mrBliss)
* Edsko de Vries (@edsko)
* Kostas Dermentzis (@kderme)
* Alfredo di Napoli (@adinapoli-iohk)

