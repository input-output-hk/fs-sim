{-# OPTIONS_GHC -Wno-orphans #-}

module Test.System.FS.Sim.Error (tests) where

import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad (unless, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import           Data.Primitive (newPinnedByteArray)
import           Data.Word
import           System.FS.API
import qualified System.FS.API.Lazy as Lazy
import           System.FS.API.Lazy (hGetExactlyAt)
import qualified System.FS.API.Strict as Strict
import           System.FS.Sim.Error
import qualified System.FS.Sim.MockFS as MockFS
import           System.FS.Sim.MockFS (HandleMock)
import qualified System.FS.Sim.Stream as Stream
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util
import           Test.Util.WithEntryCounter

-- For simplicity:
-- * We always use the full bytestring that is an argument to a property.
-- * File offsets are always 0.
-- * Byte counts are always the length of the argument bytestring.
-- * Offsets into user-supplied buffers are always 0.
-- * User-supplied buffers are always precisely as large as they need to be.
tests :: TestTree
tests = testGroup "Test.System.FS.Sim.Error" [
    -- putters
      testProperty "propPutterPutsAll hPutSome (expects failure)" $ expectFailure $
        let toInput = pure
        in  propPutterPutsAll hPutSomeC hPutSome toInput
    , testProperty "propPutterPutsAll Strict.hPutAllStrict" $
        let toInput = pure
        in  propPutterPutsAll hPutSomeC Strict.hPutAllStrict toInput
    , testProperty "propPutterPutsAll Lazy.hPutAll" $
        let toInput = pure . LBS.fromStrict
        in  propPutterPutsAll hPutSomeC Lazy.hPutAll toInput
    , testProperty "propPutterPutsAll Lazy.hPut" $
        let toInput = pure . BB.byteString
        in  propPutterPutsAll hPutSomeC Lazy.hPut toInput
    , testProperty "propPutterPutsAll hPutBufSome (expects failure)" $ expectFailure $ \p bs ->
        let put hfs h mba = fromIntegral <$>
                hPutBufSome hfs h mba 0 (fromIntegral $ BS.length bs)
            toInput _ = do
                mba <- newPinnedByteArray (BS.length bs)
                success <- MockFS.intoBuffer mba 0 bs
                unless success $ error "toInput: should not fail"
                pure mba
        in  propPutterPutsAll hPutBufSomeC put toInput p bs
    , testProperty "propPutterPutsAll hPutBufSomeAt (expects failure)" $ expectFailure $ \p bs ->
        let put hfs h mba = fromIntegral <$>
                hPutBufSomeAt hfs h mba 0 (fromIntegral $ BS.length bs) 0
            toInput _ = do
                mba <- newPinnedByteArray (BS.length bs)
                success <- MockFS.intoBuffer mba 0 bs
                unless success $ error "toInput: should not fail"
                pure mba
        in  propPutterPutsAll hPutBufSomeAtC put toInput p bs
    , testProperty "propPutterPutsAll hPutBufAll" $ \p bs ->
        let put hfs h mba = fromIntegral <$>
                hPutBufExactly hfs h mba 0 (fromIntegral $ BS.length bs)
            toInput _ = do
                mba <- newPinnedByteArray (BS.length bs)
                success <- MockFS.intoBuffer mba 0 bs
                unless success $ error "toInput: should not fail"
                pure mba
        in  propPutterPutsAll hPutBufSomeC put toInput p bs
    , testProperty "propPutterPutsAll hPutBufAllAt" $ \p bs ->
        let put hfs h mba = fromIntegral <$>
                hPutBufExactlyAt hfs h mba 0 (fromIntegral $ BS.length bs) 0
            toInput _ = do
                mba <- newPinnedByteArray (BS.length bs)
                success <- MockFS.intoBuffer mba 0 bs
                unless success $ error "toInput: should not fail"
                pure mba
        in  propPutterPutsAll hPutBufSomeAtC put toInput p bs
    -- getters
    , testProperty "propGetterGetsAll hGetSome (expect failure)" $ expectFailure $ \p bs ->
        let get hfs h  = hGetSome hfs h (fromIntegral $ BS.length bs)
            fromOutput = pure
        in  propGetterGetsAll hGetSomeC get fromOutput p bs
    , testProperty "propGetterGetsAll hGetSomeAt (expect failure)" $ expectFailure $ \p bs ->
        let get hfs h  = hGetSomeAt hfs h (fromIntegral $ BS.length bs) 0
            fromOutput = pure
        in  propGetterGetsAll hGetSomeC get fromOutput p bs
    , testProperty "propGetterGetsAll Lazy.hGetExactly" $ \p bs ->
        let get hfs h  = Lazy.hGetExactly hfs h (fromIntegral $ BS.length bs)
            fromOutput = pure . LBS.toStrict
        in  propGetterGetsAll hGetSomeC get fromOutput p bs
    , testProperty "propGetterGetsAll Lazy.hGetExactlyAt" $ \p bs ->
        let get hfs h  = Lazy.hGetExactlyAt hfs h (fromIntegral $ BS.length bs) 0
            fromOutput = pure . LBS.toStrict
        in  propGetterGetsAll hGetSomeAtC get fromOutput p bs
    , testProperty "propGetterGetsAll Lazy.hGetAll" $ \p bs ->
        let get        = Lazy.hGetAll
            fromOutput = pure . LBS.toStrict
        in  propGetterGetsAll hGetSomeC get fromOutput p bs
    , testProperty "propGetterGetsAll Lazy.hGetAllAt" $ \p bs ->
        let get hfs h  = Lazy.hGetAllAt hfs h 0
            fromOutput = pure . LBS.toStrict
        in  propGetterGetsAll hGetSomeAtC get fromOutput p bs
    , testProperty "propGetterGetsAll hGetBufSome (expects failure)" $ expectFailure $ \p bs ->
        let get hfs h = do
                mba <- newPinnedByteArray (BS.length bs)
                void $ hGetBufSome hfs h mba 0 (fromIntegral $ BS.length bs)
                pure mba
            fromOutput mba = do
                MockFS.fromBuffer mba 0 (fromIntegral $ BS.length bs) >>=
                  maybe (error "fromOutput: should not fail") pure
        in  propGetterGetsAll hGetBufSomeC get fromOutput p bs
    , testProperty "propGetterGetsAll hGetBufSomeAt (expects failure)" $ expectFailure $ \p bs ->
        let get hfs h = do
                mba <- newPinnedByteArray (BS.length bs)
                void $ hGetBufSomeAt hfs h mba 0 (fromIntegral $ BS.length bs) 0
                pure mba
            fromOutput mba = do
                MockFS.fromBuffer mba 0 (fromIntegral $ BS.length bs) >>=
                  maybe (error "fromOutput: should not fail") pure
        in  propGetterGetsAll hGetBufSomeAtC get fromOutput p bs
    , testProperty "propGetterGetsAll hGetBufExactly" $ \p bs ->
        let get hfs h = do
                mba <- newPinnedByteArray (BS.length bs)
                void $ hGetBufExactly hfs h mba 0 (fromIntegral $ BS.length bs)
                pure mba
            fromOutput mba = do
                MockFS.fromBuffer mba 0 (fromIntegral $ BS.length bs) >>=
                  maybe (error "fromOutput: should not fail") pure
        in  propGetterGetsAll hGetBufSomeC get fromOutput p bs
    , testProperty "propGetterGetsAll hGetBufExactlyAt" $ \p bs ->
        let get hfs h = do
                mba <- newPinnedByteArray (BS.length bs)
                void $ hGetBufExactlyAt hfs h mba 0 (fromIntegral $ BS.length bs) 0
                pure mba
            fromOutput mba = do
                MockFS.fromBuffer mba 0 (fromIntegral $ BS.length bs) >>=
                  maybe (error "fromOutput: should not fail") pure
        in  propGetterGetsAll hGetBufSomeAtC get fromOutput p bs
    ]

instance Arbitrary BS.ByteString where
  arbitrary = BS.pack <$> arbitrary
  shrink = fmap BS.pack . shrink . BS.unpack

{-------------------------------------------------------------------------------
  Read functions get all requested bytes
-------------------------------------------------------------------------------}

newtype SometimesPartialWrites = SometimesPartialWrites {
    getSometimesPartialWrites :: ErrorStreamPutSome
  }
  deriving Show

instance Arbitrary SometimesPartialWrites where
  arbitrary = SometimesPartialWrites <$> Stream.genInfinite (fmap Right <$> (arbitrary :: Gen (Maybe Partial)))
  shrink = fmap SometimesPartialWrites .  Stream.shrinkStream . getSometimesPartialWrites

type GetCounter = EntryCounters (StrictTVar IO) -> StrictTVar IO Word64
type PutFunction input = HasFS IO HandleMock -> Handle HandleMock -> input -> IO Word64
type ToInput input = BS.ByteString -> IO input

propPutterPutsAll ::
     GetCounter
  -> PutFunction input
  -> ToInput input
  -> SometimesPartialWrites
  -> BS.ByteString
  -> Property
propPutterPutsAll getCounter put toInput (SometimesPartialWrites errStream) bs =
    ioProperty $ do
      fsVar <- newTMVarIO MockFS.empty
      errVar <- newTVarIO onlyPutErrors
      counters <- zeroEntryCounters
      let hfs = withEntryCounters counters $ mkSimErrorHasFS fsVar errVar
      withFile hfs (mkFsPath ["file1"]) (ReadWriteMode MustBeNew) $ \h -> do
        inp <- toInput bs
        n' <- put hfs h inp
        let n = fromIntegral $ BS.length bs
        bs' <- LBS.toStrict <$> hGetExactlyAt hfs h n 0
        putN <- readTVarIO (getCounter counters)
        pure $ checkCoverage
            $ tabulate "number of writes (>1 indicates partial writes)"
                        [showPowersOf 2 $ fromIntegral putN]
            . cover 0.60 (putN > 1) "At least one partial write"
            $ n === n' .&&. bs === bs'
  where
    onlyPutErrors = emptyErrors {
        hPutSomeE = errStream
      , hPutBufSomeE = errStream
      , hPutBufSomeAtE = errStream
      }

{-------------------------------------------------------------------------------
  Write functions put all requested bytes
-------------------------------------------------------------------------------}

newtype SometimesPartialReads = SometimesPartialReads {
    getSometimesPartialReads :: ErrorStreamGetSome
  }
  deriving Show

instance Arbitrary SometimesPartialReads where
  arbitrary = SometimesPartialReads <$> Stream.genInfinite (fmap Right <$> (arbitrary :: Gen (Maybe Partial)))
  shrink = fmap SometimesPartialReads .  Stream.shrinkStream . getSometimesPartialReads

type GetFunction output = HasFS IO HandleMock -> Handle HandleMock -> IO output
type FromOutput output = output -> IO BS.ByteString

propGetterGetsAll ::
     GetCounter
  -> GetFunction output
  -> FromOutput output
  -> SometimesPartialReads
  -> BS.ByteString
  -> Property
propGetterGetsAll getCounter get fromOutput (SometimesPartialReads errStream) bs =
    ioProperty $ do
      fsVar <- newTMVarIO MockFS.empty
      errVar <- newTVarIO onlyGetErrors
      counters <- zeroEntryCounters
      let hfs = withEntryCounters counters $ mkSimErrorHasFS fsVar errVar
      withFile hfs (mkFsPath ["file1"]) (ReadWriteMode MustBeNew) $ \h -> do
        n' <- Strict.hPutAllStrict hfs h bs
        let n = fromIntegral $ BS.length bs
        hSeek hfs h AbsoluteSeek 0
        outp <- get hfs h
        bs' <- fromOutput outp
        getN <- readTVarIO (getCounter counters)
        pure $ checkCoverage
            $ tabulate "number of reads (>1 indicates partial reads)"
                        [showPowersOf 2 $ fromIntegral getN]
            . cover 60 (getN > 1) "At least one partial read"
            $ n === n' .&&. bs === bs'
  where
    onlyGetErrors = emptyErrors {
        hGetSomeE = errStream
      , hGetSomeAtE = errStream
      , hGetBufSomeE = errStream
      , hGetBufSomeAtE = errStream
      }
