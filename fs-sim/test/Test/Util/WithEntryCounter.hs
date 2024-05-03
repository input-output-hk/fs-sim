{-# LANGUAGE RecordWildCards #-}

module Test.Util.WithEntryCounter (
    EntryCounters (..)
  , zeroEntryCounters
  , incrTVar
  , withEntryCounters
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict
import           Data.Word
import           System.FS.API

data EntryCounters f = EntryCounters {
    dumpStateC                :: f Word64
    -- file operations
  , hOpenC                    :: f Word64
  , hCloseC                   :: f Word64
  , hIsOpenC                  :: f Word64
  , hSeekC                    :: f Word64
  , hGetSomeC                 :: f Word64
  , hGetSomeAtC               :: f Word64
  , hPutSomeC                 :: f Word64
  , hTruncateC                :: f Word64
  , hGetSizeC                 :: f Word64
    -- directory operations
  , createDirectoryC          :: f Word64
  , createDirectoryIfMissingC :: f Word64
  , listDirectoryC            :: f Word64
  , doesDirectoryExistC       :: f Word64
  , doesFileExistC            :: f Word64
  , removeDirectoryRecursiveC :: f Word64
  , removeFileC               :: f Word64
  , renameFileC               :: f Word64
    -- file I\/O with user-supplied buffers
  , hGetBufSomeC              :: f Word64
  , hGetBufSomeAtC            :: f Word64
  , hPutBufSomeC              :: f Word64
  , hPutBufSomeAtC            :: f Word64
  }

zeroEntryCounters :: MonadSTM m => m (EntryCounters (StrictTVar m))
zeroEntryCounters = EntryCounters <$>
    newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO 0 <*>
    newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO 0 <*>
    newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO 0 <*>
    newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO 0 <*>
    newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO 0 <*>
    newTVarIO 0 <*> newTVarIO 0

incrTVar :: MonadSTM m => StrictTVar m Word64 -> m ()
incrTVar var = atomically $ modifyTVar var (+1)

withEntryCounters ::
     MonadSTM m
  => EntryCounters (StrictTVar m)
  -> HasFS m h
  -> HasFS m h
withEntryCounters EntryCounters{..} HasFS{..} = HasFS {
      dumpState = incrTVar dumpStateC >> dumpState
      -- file operatoins
    , hOpen = \a b -> incrTVar hOpenC >> hOpen a b
    , hClose = \a -> incrTVar hCloseC >> hClose a
    , hIsOpen = \a -> incrTVar hIsOpenC >> hIsOpen a
    , hSeek = \a b c -> incrTVar hSeekC >> hSeek a b c
    , hGetSome = \a b -> incrTVar hGetSomeC >> hGetSome a b
    , hGetSomeAt = \a b c -> incrTVar hGetSomeAtC >> hGetSomeAt a b c
    , hPutSome = \a b -> incrTVar hPutSomeC >> hPutSome a b
    , hTruncate = \a b -> incrTVar hTruncateC >> hTruncate a b
    , hGetSize = \a -> incrTVar hGetSizeC >> hGetSize a
      -- directory operations
    , createDirectory = \a -> incrTVar createDirectoryC >> createDirectory a
    , createDirectoryIfMissing = \a b -> incrTVar createDirectoryIfMissingC >> createDirectoryIfMissing a b
    , listDirectory = \a -> incrTVar listDirectoryC >> listDirectory a
    , doesDirectoryExist = \a -> incrTVar doesDirectoryExistC >> doesDirectoryExist a
    , doesFileExist = \a -> incrTVar doesFileExistC >> doesFileExist a
    , removeDirectoryRecursive = \a -> incrTVar removeDirectoryRecursiveC >> removeDirectoryRecursive a
    , removeFile = \a -> incrTVar removeFileC >> removeFile a
    , renameFile = \a b -> incrTVar renameFileC >> renameFile a b
    , mkFsErrorPath = mkFsErrorPath
    , unsafeToFilePath = unsafeToFilePath
      -- file I\/O with user-supplied buffers
    , hGetBufSome = \a b c d -> incrTVar hGetBufSomeC >> hGetBufSome a b c d
    , hGetBufSomeAt = \a b c d e -> incrTVar hGetBufSomeAtC >> hGetBufSomeAt a b c d e
    , hPutBufSome = \a b c d -> incrTVar hPutBufSomeC >> hPutBufSome a b c d
    , hPutBufSomeAt = \a b c d e -> incrTVar hPutBufSomeAtC >> hPutBufSomeAt a b c d e
    }
