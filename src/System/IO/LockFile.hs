{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module:       $HEADER$
-- Description:  Provide exclusive access to a resource using lock file.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (CPP, DeriveDataTypeable)
--
-- Provide exclusive access to a resource using lock file.
module System.IO.LockFile
    (
    -- * Run computation with locked resource.
      withLockFile
    , withLockFile_
    , withLockFile'

    -- * Configuration
    , LockingParameters(..)
    , RetryStrategy(..)

    -- * Exceptions
    , LockingException(..)

    -- * Utility functions
    , withLockExt
    )
    where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Word (Word8, Word64)
import System.IO
    (Handle, IOMode(WriteMode), hClose, hFlush, hPutStrLn, openFile)
import System.Posix.Internals (c_getpid)

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.TaggedException
    ( Exception
    , MonadException(throw), onException'
#if MIN_VERSION_tagged_exception_core(1,1,0)
    , MonadExceptionUtilities(mask')
#endif
    , Throws
    , liftT
    , mapException
    )
#if MIN_VERSION_tagged_exception_core(1,1,0)
import Control.Monad.TaggedException.Hidden (HiddenException, hide)
#else
import Control.Monad.TaggedException.Hidden (HidableException, hide)
import Control.Monad.TaggedException.Utilities
    (MonadExceptionUtilities(mask'))
#endif
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
    -- ^ Wasn't able to aquire lock file specified as an argument.
    | CaughtIOException IOException
    -- ^ 'IOException' occurred while creating or removing lock file.
  deriving (Typeable)

instance Show LockingException where
    showsPrec _ e = case e of
        UnableToAcquireLockFile fp -> shows' "Unable to acquire lock file" fp
        CaughtIOException ioe -> shows' "Caught IO exception" ioe
      where shows' str x = showString str . showString ": " . shows x

instance Exception LockingException
#if MIN_VERSION_tagged_exception_core(1,1,0)
instance HiddenException LockingException
#else
instance HidableException LockingException
#endif

-- | This function should not be exported!
--
-- Map 'IOException' to 'LockingException'.
wrapIOException
    :: (MonadException m)
    => Throws IOException m a -> Throws LockingException m a
wrapIOException = mapException CaughtIOException

-- | This function should not be exported!
--
-- Lift @IO@ and map any raised 'IOException' to 'LockingException'.
io :: (MonadException m, MonadIO m) => IO a -> Throws LockingException m a
io = wrapIOException . liftIO

-- | This function should not be exported!
--
-- Open lock file write PID of a current process in to it and return its handle.
--
-- If operation doesn't succeed, then 'LockingException' is raised. See also
-- 'LockingParameters' and 'RetryStrategy' for details.
lock
    :: (MonadException m, MonadIO m)
    => LockingParameters
    -> FilePath
    -> Throws LockingException m Handle
lock params = lock' $ case retryToAcquireLock params of
    NumberOfTimes 0 -> params{retryToAcquireLock = No}
    _ -> params
  where
    lock' params' lockFileName
      | retryToAcquireLock params' == NumberOfTimes 0 = failedToAcquireLockFile
      | otherwise = do
        lockAlreadyExist <- io $ doesFileExist lockFileName
        if lockAlreadyExist
            then case retryToAcquireLock params' of
                No -> failedToAcquireLockFile
                _ -> do
                    io $ threadDelay sleepBetweenRetires'
                    lock' paramsDecRetries lockFileName
            else io $ do
                -- TODO
                -- openFile is not safe like this, we need to fail in case that
                -- file already exist.
                lockFileHandle <- openFile lockFileName WriteMode
                c_getpid >>= hPutStrLn lockFileHandle . ("PID=" ++) . show
                hFlush lockFileHandle
                return lockFileHandle
      where
        sleepBetweenRetires' = fromIntegral $ sleepBetweenRetires params'
        failedToAcquireLockFile = throw $ UnableToAcquireLockFile lockFileName

        paramsDecRetries = case retryToAcquireLock params' of
            NumberOfTimes n ->
                params'{retryToAcquireLock = NumberOfTimes $ n - 1}
            _ -> params'

-- | This function should not be exported!
--
-- Close lock file handle and then delete it.
unlock
    :: (MonadException m, MonadIO m)
    => FilePath
    -> Handle
    -> Throws LockingException m ()
unlock lockFileName lockFileHandle =
    io $ hClose lockFileHandle >> removeFile lockFileName

-- | Acquire a lock file before running computation and release it when it's
-- done.
--
-- If \"action\" raises 'IOException' then this is not wrapped by
-- 'LockingException'. Only 'IOException' that occurred during locking or
-- unlocking is mapped to 'LockingException'.
withLockFile
    :: (MonadExceptionUtilities m, MonadIO m)
    => LockingParameters
    -> FilePath
    -- ^ Lock file name.
    -> m a
    -> Throws LockingException m a
withLockFile params lockFileName action = mask' $ \ restore -> do
    lockFileHandle <- lock params lockFileName
    r <- restore (liftT action)
        `onException'` unlock lockFileName lockFileHandle
    _ <- unlock lockFileName lockFileHandle
    return r

-- | Type restricted version of 'withLockFile'.
withLockFile_
    :: (MonadExceptionUtilities m, MonadIO m)
    => LockingParameters
    -> FilePath
    -- ^ Lock file name.
    -> m ()
    -> Throws LockingException m ()
withLockFile_ = withLockFile

-- | Version of 'withLockFile' that hides exception witness from type its
-- signature.
withLockFile'
    :: (MonadExceptionUtilities m, MonadIO m)
    => LockingParameters
    -> FilePath
    -- ^ Lock file name.
    -> m a
    -> m a
withLockFile' = ((hide .) .) . withLockFile
