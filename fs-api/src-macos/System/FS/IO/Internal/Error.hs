module System.FS.IO.Internal.Error (sameError) where

import           System.FS.API.Types (FsError (..), FsErrorType (..),
                     sameFsError)

-- Check default implementation first using 'sameFsError', and otherwise permit
-- some combinations of error types that are not structurally equal.
sameError :: FsError -> FsError -> Bool
sameError e1 e2 = sameFsError e1 e2
               || (fsErrorPath e1 == fsErrorPath e2
               && permitted (fsErrorType e1) (fsErrorType e2))
  where
    -- error types that are permitted to differ for technical reasons
    permitted ty1 ty2 = case (ty1, ty2) of
      (FsInsufficientPermissions  , FsResourceInappropriateType) -> True
      (FsResourceInappropriateType, FsInsufficientPermissions  ) -> True
      (_                          , _                          ) -> False
