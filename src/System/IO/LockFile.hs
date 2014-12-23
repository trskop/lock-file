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
    -- * Usage Example
    -- $usageExample

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
-- unlocking is mapped to 'LockingException'. This doesn't affect the fact
-- that lock file is removed even if \"action\" fails.
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

-- | Version of 'withLockFile' that hides exception witness from its type
-- signature.
withLockFile'
    :: (MonadMask m, MonadIO m)
    => LockingParameters
    -> FilePath
    -- ^ Lock file name.
    -> m a
    -> m a
withLockFile' = ((hideException .) .) . withLockFile

-- $usageExample
--
-- Following example acquires lock file and then waits @1000000@ micro seconds
-- before releasing it. Note also that it is possible to specify retry
-- strategy. Here we set it to 'No' and therefore this code won't retry to
-- acquire lock file after first failure.
--
-- @
-- module Main (main)
--     where
--
-- import Control.Concurrent (threadDelay)
--     -- From base package, but GHC specific.
--
-- import qualified Control.Monad.TaggedException as Exception (handle)
--     -- From tagged-exception-core package.
--     -- <http://hackage.haskell.org/package/tagged-exception-core>
-- import Data.Default.Class (Default(def))
--     -- From data-default-class package, alternatively it's possible to use
--     -- data-default package version 0.5.2 and above.
--     -- <http://hackage.haskell.org/package/data-default-class>
--     -- <http://hackage.haskell.org/package/data-default>
-- import "System.IO.LockFile"
--     ( 'LockingParameters'('retryToAcquireLock')
--     , 'RetryStrategy'('No')
--     , 'withLockFile'
--     )
--
--
-- main :: IO ()
-- main = handleException
--     . 'withLockFile' lockParams lockFile $ threadDelay 1000000
--   where
--     lockParams = def
--         { 'retryToAcquireLock' = 'No'
--         }
--
--     lockFile = \"\/var\/run\/lock\/my-example-lock\"
--
--     handleException = Exception.handle
--         $ putStrLn . ("Locking failed with: " ++) . show
-- @
--
-- This command line example shows that trying to execute two instances of
-- `example` at the same time will result in failure of the second one.
--
-- > $ ghc example.hs
-- > [1 of 1] Compiling Main             ( example.hs, example.o )
-- > Linking example ...
-- > $ ./example & ./example
-- > [1] 7893
-- > Locking failed with: Unable to acquire lock file: "/var/run/lock/my-example-lock"
-- > $ [1]+  Done                    ./example
