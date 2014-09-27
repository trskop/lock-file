{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Low-level API for providing exclusive access to a resource
--               using lock file.
-- Copyright:    (c) 2013, 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (CPP, DeriveDataTypeable, NoImplicitPrelude)
--
-- Low-level API for providing exclusive access to a resource using lock file.
module System.IO.LockFile.Internal
    (
    -- * Locking primitives
      lock
    , unlock

    -- * Configuration
    , LockingParameters(..)
    , RetryStrategy(..)

    -- * Exceptions
    , LockingException(..)
    )
    where

import Prelude (Num((-)), fromIntegral)

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Exception (Exception, IOException, ioError)
import Control.Monad (Monad((>>), (>>=), return), when)
import Data.Bits ((.|.))
import Data.Bool (otherwise)
import Data.Data (Data)
import Data.Eq (Eq((==), (/=)))
import Data.Ord (Ord((>)))
import Data.Function ((.), ($))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Typeable (Typeable)
import Data.Word (Word8, Word64)
import Foreign.C (eEXIST, errnoToIOError, getErrno)
import GHC.IO.Handle.FD (fdToHandle)
import System.IO (FilePath, Handle, IO, hClose, hFlush, hPutStr)
import System.Posix.Internals
    ( c_close
    , c_getpid
    , c_open
    , o_BINARY
    , o_CREAT
    , o_EXCL
    , o_NOCTTY
    , o_NONBLOCK
    , o_RDWR
    , withFilePath
    )
import Text.Read (Read)
import Text.Show (Show(showsPrec), show, shows, showString)

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Directory (removeFile)

import Control.Monad.TaggedException
    ( Throws
    , throw
    , mapException
    , onException'
    )
import Control.Monad.TaggedException.Hidden (HiddenException)
import Data.Default.Class (Default(def))


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
    { retryToAcquireLock :: !RetryStrategy
    , sleepBetweenRetires :: !Word64
    -- ^ Sleep interval is in microseconds.
    }
  deriving (Data, Eq, Show, Read, Typeable)

-- | @def = 'LockingParameters' def 8000000@
--
-- Sleep interval is inspired by @lockfile@ command line utility that is part
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
instance HiddenException LockingException

-- | Map 'IOException' to 'LockingException'.
wrapIOException
    :: (MonadMask m)
    => Throws IOException m a
    -> Throws LockingException m a
wrapIOException = mapException CaughtIOException

-- | Lift @IO@ and map any raised 'IOException' to 'LockingException'.
io :: (MonadMask m, MonadIO m) => IO a -> Throws LockingException m a
io = wrapIOException . liftIO

-- | Open lock file write PID of a current process in to it and return its
-- handle.
--
-- If operation doesn't succeed, then 'LockingException' is raised. See also
-- 'LockingParameters' and 'RetryStrategy' for details.
lock
    :: (MonadMask m, MonadIO m)
    => LockingParameters
    -> FilePath
    -> Throws LockingException m Handle
lock params = lock' $ case retryToAcquireLock params of
    NumberOfTimes 0 -> params{retryToAcquireLock = No}
    _ -> params
  where
    openLockFile lockFileName = io $ do
        fd <- withFilePath lockFileName $ \ fp -> c_open fp openFlags 0o644
        if fd > 0
            then Just <$> fdToHandle fd `onException'` c_close fd
            else do
                errno <- getErrno
                when (errno /= eEXIST) . ioError
                    . errnoToIOError "lock" errno Nothing $ Just lockFileName
                -- Failed to open lock file because it already exists
                return Nothing
      where
        openFlags = o_NONBLOCK .|. o_NOCTTY .|. o_RDWR .|. o_CREAT .|. o_EXCL
            .|. o_BINARY

    lock' params' lockFileName
      | retryToAcquireLock params' == NumberOfTimes 0 = failedToAcquireLockFile
      | otherwise = do
            lockFileHandle <- openLockFile lockFileName
            case lockFileHandle of
                Nothing -> case retryToAcquireLock params' of
                    No -> failedToAcquireLockFile
                    _ -> do
                        io $ threadDelay sleepBetweenRetires'
                        lock' paramsDecRetries lockFileName
                Just h -> io $ do
                    c_getpid >>= hPutStr h . show
                    hFlush h
                    return h
      where
        sleepBetweenRetires' = fromIntegral $ sleepBetweenRetires params'
        failedToAcquireLockFile = throw $ UnableToAcquireLockFile lockFileName

        paramsDecRetries = case retryToAcquireLock params' of
            NumberOfTimes n ->
                params'{retryToAcquireLock = NumberOfTimes $ n - 1}
            _ -> params'

-- | Close lock file handle and then delete it.
unlock
    :: (MonadMask m, MonadIO m)
    => FilePath
    -> Handle
    -> Throws LockingException m ()
unlock lockFileName lockFileHandle =
    io $ hClose lockFileHandle >> removeFile lockFileName
