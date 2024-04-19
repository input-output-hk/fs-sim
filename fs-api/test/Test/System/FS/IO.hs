{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.System.FS.IO (tests) where

import           Control.Monad.Primitive
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short.Internal as SBS
import           Data.Primitive.ByteArray
import           Prelude hiding (read)
import qualified System.FS.API as FS
import qualified System.FS.IO as IO
import           System.IO.Temp
import           System.Posix.Types (ByteCount)
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Test.System.FS.IO" [
      testProperty "prop_roundtrip_hPutGetBufSome"
        prop_roundtrip_hPutGetBufSome
    , testProperty "prop_roundtrip_hPutGetBufSomeAt"
        prop_roundtrip_hPutGetBufSomeAt
    , testProperty "prop_roundtrip_hPutGetBufExactly"
        prop_roundtrip_hPutGetBufExactly
    , testProperty "prop_roundtrip_hPutGetBufExactlyAt"
        prop_roundtrip_hPutGetBufExactlyAt
    ]

instance Arbitrary ByteString where
  arbitrary = BS.pack <$> arbitrary
  shrink = fmap BS.pack . shrink . BS.unpack

instance Arbitrary FS.AbsOffset where
  arbitrary = FS.AbsOffset . getSmall <$> arbitrary
  shrink (FS.AbsOffset x) = FS.AbsOffset <$> shrink x

fromByteString :: PrimMonad m => ByteString -> m (MutableByteArray (PrimState m))
fromByteString bs = thawByteArray (ByteArray ba) 0 (SBS.length sbs)
  where !sbs@(SBS.SBS ba) = SBS.toShort bs

toByteString :: PrimMonad m => Int -> MutableByteArray (PrimState m) -> m ByteString
toByteString n mba = freezeByteArray mba 0 n >>= \(ByteArray ba) -> pure (SBS.fromShort $ SBS.SBS ba)

-- | A write-then-read roundtrip test for buffered I\/O in 'IO'.
--
-- The 'ByteString'\'s internal pointer doubles as the buffer used for the I\/O
-- operations, and we only write/read a prefix of the bytestring. This does not
-- test what happens if we try to write/read more bytes than fits in the buffer,
-- because the behaviour is then undefined.
prop_roundtrip_hPutGetBufSome ::
     ByteString
  -> Small ByteCount -- ^ Prefix length
  -> Property
prop_roundtrip_hPutGetBufSome bs (Small c) =
  BS.length bs >= fromIntegral c ==>
  ioProperty $ withSystemTempDirectory "prop_roundtrip_hPutGetBufSome" $ \dirPath -> do
    let hfs  = IO.ioHasFS (FS.MountPoint dirPath)
        hbfs = IO.ioHasBufFS (FS.MountPoint dirPath)

    FS.withFile hfs (FS.mkFsPath ["temp"]) (FS.WriteMode FS.MustBeNew) $ \h -> do
      putBuf <- fromByteString bs
      m <- FS.hPutBufSome hbfs h putBuf 0 c
      let writeTest = counterexample "wrote too many bytes" ((if c > 0 then 1 .<= m else property True) .&&. m .<= c)
      FS.hSeek hfs h FS.AbsoluteSeek 0
      getBuf <- newPinnedByteArray (fromIntegral m)
      o <- FS.hGetBufSome hfs h getBuf 0 m
      let readTest = counterexample "read too many bytes"   ((if c > 0 then 1 .<= o else property True) .&&. o .<= m)
      bs' <- toByteString (fromIntegral o) getBuf
      let cmpTest = counterexample "(prefix of) input and output bytestring do not match"
                  $ BS.take (fromIntegral o) bs === bs'
      pure (writeTest .&&. readTest .&&. cmpTest)

-- | Like 'prop_roundtrip_hPutGetBufSome', but reading and writing at a specified offset.
prop_roundtrip_hPutGetBufSomeAt ::
     ByteString
  -> Small ByteCount -- ^ Prefix length
  -> FS.AbsOffset
  -> Property
prop_roundtrip_hPutGetBufSomeAt bs (Small c) off =
  BS.length bs >= fromIntegral c ==>
  ioProperty $ withSystemTempDirectory "prop_roundtrip_hPutGetBufSomeAt" $ \dirPath -> do
    let hfs  = IO.ioHasFS (FS.MountPoint dirPath)
        hbfs = IO.ioHasBufFS (FS.MountPoint dirPath)

    FS.withFile hfs (FS.mkFsPath ["temp"]) (FS.WriteMode FS.MustBeNew) $ \h -> do
      putBuf <- fromByteString bs
      m <- FS.hPutBufSomeAt hbfs h putBuf 0 c off
      let writeTest = counterexample "wrote too many bytes" ((if c > 0 then 1 .<= m else property True) .&&. m .<= c)
      getBuf <- newPinnedByteArray (fromIntegral m)
      o <- FS.hGetBufSomeAt hfs h getBuf 0 m off
      let readTest = counterexample "read too many bytes"   ((if c > 0 then 1 .<= o else property True) .&&. o .<= m)
      bs' <- toByteString (fromIntegral o) getBuf
      let cmpTest = counterexample "(prefix of) input and output bytestring do not match"
                  $ BS.take (fromIntegral o) bs === bs'
      pure (writeTest .&&. readTest .&&. cmpTest)

-- | Like 'prop_roundtrip_hPutGetBufSome', but ensuring that all bytes are
-- written/read.
prop_roundtrip_hPutGetBufExactly ::
     ByteString
  -> Small ByteCount -- ^ Prefix length
  -> Property
prop_roundtrip_hPutGetBufExactly bs (Small c) =
  BS.length bs >= fromIntegral c ==>
  ioProperty $ withSystemTempDirectory "prop_roundtrip_hPutGetBufExactly" $ \dirPath -> do
    let hfs  = IO.ioHasFS (FS.MountPoint dirPath)
        hbfs = IO.ioHasBufFS (FS.MountPoint dirPath)

    FS.withFile hfs (FS.mkFsPath ["temp"]) (FS.WriteMode FS.MustBeNew) $ \h -> do
      putBuf <- fromByteString bs
      m <- FS.hPutBufExactly hbfs h putBuf 0 c
      let writeTest = counterexample "wrote too few bytes" (m === c)
      FS.hSeek hfs h FS.AbsoluteSeek 0
      getBuf <- newPinnedByteArray (fromIntegral c)
      o <- FS.hGetBufExactly hfs h getBuf 0 c
      let readTest = counterexample "read too few byes"    (o === c)
      bs' <- toByteString (fromIntegral c) getBuf
      let cmpTest = counterexample "input and output bytestring do not match"
                  $ BS.take (fromIntegral c) bs === BS.take (fromIntegral c) bs'
      pure (writeTest .&&. readTest .&&. cmpTest)

-- | Like 'prop_roundtrip_hPutGetBufSome', but reading and writing at a
-- specified offset, and ensuring that all bytes are written/read.
prop_roundtrip_hPutGetBufExactlyAt ::
     ByteString
  -> Small ByteCount -- ^ Prefix length
  -> FS.AbsOffset
  -> Property
prop_roundtrip_hPutGetBufExactlyAt bs (Small c) off =
  BS.length bs >= fromIntegral c ==>
  ioProperty $ withSystemTempDirectory "prop_roundtrip_hPutGetBufExactlyAt" $ \dirPath -> do
    let hfs  = IO.ioHasFS (FS.MountPoint dirPath)
        hbfs = IO.ioHasBufFS (FS.MountPoint dirPath)

    FS.withFile hfs (FS.mkFsPath ["temp"]) (FS.WriteMode FS.MustBeNew) $ \h -> do
      putBuf <- fromByteString bs
      m <- FS.hPutBufExactlyAt hbfs h putBuf 0 c off
      let writeTest = counterexample "wrote too few bytes" (m === c)
      getBuf <- newPinnedByteArray (fromIntegral c)
      o <- FS.hGetBufExactlyAt hfs h getBuf 0 c off
      let readTest = counterexample "read too few byes"    (o === c)
      bs' <- toByteString (fromIntegral c) getBuf
      let cmpTest = counterexample "input and output bytestring do not match"
                  $ BS.take (fromIntegral c) bs === BS.take (fromIntegral c) bs'
      pure (writeTest .&&. readTest .&&. cmpTest)

infix 4 .<=

(.<=) :: (Ord a, Show a) => a -> a -> Property
x .<= y = counterexample (show x ++ interpret res ++ show y) res
  where
    res             = x <= y
    interpret True  = " <= "
    interpret False = " > "
