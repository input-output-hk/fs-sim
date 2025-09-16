{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Support for CRC
module System.FS.CRC (
    -- * Wrap digest functionality
    CRC (..),
    computeCRC,
    initCRC,
    updateCRC,

    -- * File system functions with CRC functionality
    hGetAllAtCRC,
    hGetExactlyAtCRC,
    hPutAllCRC,
) where

import Control.Monad (foldM)
import Control.Monad.Class.MonadThrow
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BSS
import qualified Data.Digest.CRC32C as Digest
import Data.Word
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GHC.Stack
import System.FS.API.Lazy
import System.FS.API.Strict

{-------------------------------------------------------------------------------
  Wrap functionality from digest
-------------------------------------------------------------------------------}

newtype CRC = CRC {getCRC :: Word32}
    deriving (Eq, Show, Generic, Storable)

initCRC :: CRC
initCRC = CRC $ Digest.crc32c BS.empty

updateCRC :: forall a. (CRC32C a) => a -> CRC -> CRC
updateCRC a = CRC . flip crc32cUpdate a . getCRC

computeCRC :: forall a. (CRC32C a) => a -> CRC
computeCRC = CRC . crc32c

class CRC32C a where
    crc32c :: a -> Word32
    crc32c = crc32cUpdate 0

    crc32cUpdate :: Word32 -> a -> Word32

instance CRC32C BS.ByteString where
    crc32c bs = Digest.crc32c bs

    crc32cUpdate cks bs = Digest.crc32c_update cks bs

instance CRC32C BL.ByteString where
    crc32cUpdate = BL.foldlChunks crc32cUpdate

instance CRC32C [Word8] where
    crc32cUpdate n = (crc32cUpdate n) . BL.pack

instance CRC32C BSS.ShortByteString where
    crc32c = crc32c . BSS.fromShort

    crc32cUpdate cks = crc32cUpdate cks . BSS.fromShort

{-------------------------------------------------------------------------------
  File system functions that compute CRCs
-------------------------------------------------------------------------------}

-- | Variation on 'hPutAll' that also computes a CRC
hPutAllCRC ::
    forall m h.
    (HasCallStack, Monad m) =>
    HasFS m h ->
    Handle h ->
    BL.ByteString ->
    m (Word64, CRC)
hPutAllCRC hasFS h = foldM putChunk (0, initCRC) . BL.toChunks
  where
    putChunk :: (Word64, CRC) -> BS.ByteString -> m (Word64, CRC)
    putChunk (written, crc) chunk = do
        chunkSize <- hPutAllStrict hasFS h chunk
        let !written' = written + chunkSize
            !crc' = updateCRC chunk crc
        return (written', crc')

-- | Variation on 'hGetExactlyAt' that also computes a CRC
hGetExactlyAtCRC ::
    forall m h.
    (HasCallStack, MonadThrow m) =>
    HasFS m h ->
    Handle h ->
    -- | The number of bytes to read.
    Word64 ->
    -- | The offset at which to read.
    AbsOffset ->
    m (BL.ByteString, CRC)
hGetExactlyAtCRC hasFS h n offset = do
    -- TODO Interleave reading with computing the CRC. Better cache locality
    -- and fits better with incremental parsing, when we add support for that.
    bs <- hGetExactlyAt hasFS h n offset
    let !crc = computeCRC bs
    return (bs, crc)

-- | Variation on 'hGetAllAt' that also computes a CRC
hGetAllAtCRC ::
    forall m h.
    (Monad m) =>
    HasFS m h ->
    Handle h ->
    -- | The offset at which to read.
    AbsOffset ->
    m (BL.ByteString, CRC)
hGetAllAtCRC hasFS h offset = do
    -- TODO Interleave reading with computing the CRC. Better cache locality
    -- and fits better with incremental parsing, when we add support for that.
    bs <- hGetAllAt hasFS h offset
    let !crc = computeCRC bs
    return (bs, crc)
