# fs-sim

The `fs-sim` package provides a filesystem simulator that facilitates
simulation of errors and file corruption. This simulator is specially useful for
testing purposes, and works well in conjunction with
[`io-sim`](ps://github.com/input-output-hk/io-sim).

Code that is written using the abstract filesystem interface (`HasFS`) that is
provided by the parent package `fs-api` can be run against any of the simulator
implementations provided by `fs-sim`. `fs-sim` currently provides three
simulators:
* `System.FS.Sim.Pure` provides a pure implementation.
* `System.FS.Sim.STM` provides an implementation that uses STM.
* `System.FS.im.Error` provides an implementation that uses STM, but can also
  simulate errors and file corruption.

```haskell
pureHasFS :: HasFS PureSimFS Mock.HandleMock

simHasFS :: forall m. (MonadSTM m, MonadThrow m)
         => StrictTVar m MockFS
         -> HasFS m HandleMock

mkSimErrorHasFS :: forall m. (MonadSTM m, MonadThrow m)
                => StrictTVar m MockFS
                -> StrictTVar m Errors
                -> HasFS m HandleMock
```
