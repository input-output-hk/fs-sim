# fs-api

The `fs-api` package provides an abstract interface to filesystems. The abstract
interface is a datatype called `HasFS`, which is parameterised over a monad `m`
that filesystem operations run in, and the type of file handles `h`.
`removeFile` is an example of a filesystem operation in this interface.

```haskell
data HasFS m h = HasFS {
    {- omitted -}
    -- | Remove the file (which must exist)
  , removeFile               :: HasCallStack => FsPath -> m ()
    {- omitted -}
  }
```

Code that is written using this interface can be run against any implementation
of a file system. The `System.FS.IO` module provides a function for initialising
a `HasFS` interface for the real filesystem.

```haskell
ioHasFS :: (MonadIO m, PrimState IO ~ PrimState m) => MountPoint -> HasFS m HandleIO
```

Note that `ioHasFS` requires some context in the form of a `MountPoint`: the
base directory in which the filesystem operations should be run.
