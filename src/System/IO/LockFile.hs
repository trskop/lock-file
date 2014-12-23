{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Provide exclusive access to a resource using lock file.
-- Copyright:    (c) 2013, 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  CPP, NoImplicitPrelude
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

import Control.Monad (Monad(return))
import Data.Function ((.), ($))
import Data.List ((++))
import System.IO (FilePath)

import Control.Monad.IO.Class (MonadIO)

import Control.Monad.Catch (MonadMask(mask))
import Control.Monad.TaggedException
    ( Throws
    , liftT
    , onException'
    )
import Control.Monad.TaggedException.Hidden (HiddenException(hideException))

import System.IO.LockFile.Internal


-- | Append default lock file extension. Useful e.g. for generating lock file
-- name out of regular file name.
withLockExt :: FilePath -> FilePath
withLockExt = (++ ".lock")

-- | Acquire a lock file before running computation and release it when it's
-- done.
--
-- If \"action\" raises 'IOException' then this is not wrapped by
-- 'LockingException'. Only 'IOException' that occurred during locking or
-- unlocking is mapped to 'LockingException'.
withLockFile
    :: (MonadMask m, MonadIO m)
    => LockingParameters
    -> FilePath
    -- ^ Lock file name.
    -> m a
    -> Throws LockingException m a
withLockFile params lockFileName action = mask $ \ restore -> do
    lockFileHandle <- lock params lockFileName
    r <- restore (liftT action)
        `onException'` unlock lockFileName lockFileHandle
    _ <- unlock lockFileName lockFileHandle
    return r

-- | Type restricted version of 'withLockFile'.
withLockFile_
    :: (MonadMask m, MonadIO m)
    => LockingParameters
    -> FilePath
    -- ^ Lock file name.
    -> m ()
    -> Throws LockingException m ()
withLockFile_ = withLockFile

-- | Version of 'withLockFile' that hides exception witness from type its
-- signature.
withLockFile'
    :: (MonadMask m, MonadIO m)
    => LockingParameters
    -> FilePath
    -- ^ Lock file name.
    -> m a
    -> m a
withLockFile' = ((hideException .) .) . withLockFile
