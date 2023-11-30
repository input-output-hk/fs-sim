module System.FS.IO.Internal.Error (sameError) where

import           System.FS.API.Types (FsError, sameFsError)

sameError :: FsError -> FsError -> Bool
sameError = sameFsError

