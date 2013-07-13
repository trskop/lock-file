{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module:       $HEADER$
-- Description:  Provide exclusive access to a resource using lock file.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (DeriveDataTypeable)
--
-- Edit file as in-place.
module System.IO.LockFile
    (
    -- * Run computation with locked resource.
      withLockFile
    , withLockFile_

    -- * Configuration
    , LockingParameters(..)
    , RetryStrategy(..)

    -- * Exceptions
    , LockingException(..)

    -- * Utility functions
    , withLockExt
    )
    where

-- import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Exception (Exception, mask, onException, throw)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Word (Word8, Word64)
import System.IO
    (Handle, IOMode(WriteMode), hClose, hFlush, hPutStrLn, openFile)
import System.Posix.Internals (c_getpid)

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Default.Class (Default(def))
import System.Directory (doesFileExist, removeFile)


-- | Append default lock file extension. Useful e.g. for generating lock file
-- name out of regular file name.
withLockExt :: FilePath -> FilePath
withLockExt = (++ ".lock")

data RetryStrategy
    = No
    -- ^ Don't retry at all.
    | Indefinitely
    -- ^ Retry indefinitely.
    | NumberOfTimes Word8
    -- ^ Retry only specified number of times.
    -- If equal to zero then it is interpreted as 'No'.
  deriving (Data, Eq, Show, Read, Typeable)

-- | @def = 'Indefinitely'@
instance Default RetryStrategy where
    def = Indefinitely

data LockingParameters = LockingParameters
    { retryToAcquireLock :: RetryStrategy
    , sleepBetweenRetires :: Word64
    -- ^ Sleep interval is in microseconds.
    }
  deriving (Data, Eq, Show, Read, Typeable)

-- | @def = 'LockingParameters' def 8000000@
--
-- Sleep interfal is inspired by @lockfile@ command line utility that is part
-- of Procmail.
instance Default LockingParameters where
    def = LockingParameters
        { retryToAcquireLock = def
        , sleepBetweenRetires = 8000000 -- 8 s
        }

data LockingException
    = UnableToAcquireLockFile FilePath
  deriving (Typeable)

instance Show LockingException where
    showsPrec _ e = case e of
        UnableToAcquireLockFile fp ->
            showString "Unable to acquire lock file: " . shows fp

instance Exception LockingException

-- | This function should not exported!
lock
    :: (MonadIO m)
    => LockingParameters
    -> FilePath
    -> {- Throws LockingException -} m Handle
lock params = (liftIO .) . lock' $ case retryToAcquireLock params of
    NumberOfTimes 0 -> params{retryToAcquireLock = No}
    _ -> params
  where
    lock' params' lockFileName
      | retryToAcquireLock params' == NumberOfTimes 0 = failedToAcquireLockFile
      | otherwise = do
        lockAlreadyExist <- doesFileExist lockFileName
        if lockAlreadyExist
            then case retryToAcquireLock params' of
                No -> failedToAcquireLockFile
                _ -> do
                    threadDelay . fromIntegral $ sleepBetweenRetires params'
                    lock' paramsDecRetries lockFileName
            else do
                lockFileHandle <- openFile lockFileName WriteMode
                c_getpid >>= hPutStrLn lockFileHandle . ("PID=" ++) . show
                hFlush lockFileHandle
                return lockFileHandle
      where
        failedToAcquireLockFile = throw $ UnableToAcquireLockFile lockFileName

        paramsDecRetries = case retryToAcquireLock params' of
            NumberOfTimes n ->
                params'{retryToAcquireLock = NumberOfTimes $ n - 1}
            _ -> params'

-- | This function should not exported!
unlock
    :: (MonadIO m)
    => FilePath
    -> Handle
    -> {- Throws LockingException -} m ()
unlock lockFileName lockFileHandle =
    liftIO $ hClose lockFileHandle >> removeFile lockFileName

-- | Acquire a lock file before running computation and release it when it's
-- done.
withLockFile
    :: {-(MonadException m, MonadIO m)
    =>-} LockingParameters
    -> FilePath
    -- ^ Lock file name.
    -> IO a
    -> IO a
withLockFile params lockFileName action = mask $ \ restore -> do
    lockFileHandle <- lock params lockFileName
    r <- restore action `onException` unlock lockFileName lockFileHandle
    _ <- unlock lockFileName lockFileHandle
    return r

-- | Type restricted version of 'withLockFile'.
withLockFile_
    :: {-(MonadException m, MonadIO m)
    =>-} LockingParameters
    -> FilePath
    -- ^ Lock file name.
    -> IO ()
    -> IO ()
withLockFile_ = withLockFile
