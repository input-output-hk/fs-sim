{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.FS.API.Strict (
    -- * API
    module API
    -- * Strict functions
  , hPutAllStrict
  ) where

import qualified Data.ByteString as BS
import           Data.Word
import           System.FS.API as API
import           Util.CallStack

-- | This function makes sure that the whole 'BS.ByteString' is written.
hPutAllStrict :: forall m h
              .  (HasCallStack, Monad m)
              => HasFS m h
              -> Handle h
              -> BS.ByteString
              -> m Word64
hPutAllStrict hasFS h = go 0
  where
    go :: Word64 -> BS.ByteString -> m Word64
    go !written bs = do
      n <- hPutSome hasFS h bs
      let bs'      = BS.drop (fromIntegral n) bs
          written' = written + n
      if BS.null bs'
        then return written'
        else go written' bs'
