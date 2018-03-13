{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Provide exclusive access to a resource using lock file.
-- Copyright:    (c) 2013-2015, 2018 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Provide exclusive access to a resource using lock file.
module System.IO.LockFile
    (
    -- * Usage Example
    -- $usageExample

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

import Control.Applicative (pure)
import Control.Monad.IO.Class (MonadIO)
import Data.Function ((.), ($))
import Data.Functor (void)
import Data.List ((++))
import System.IO (FilePath)

import Control.Monad.Catch (MonadMask, mask, onException)

import System.IO.LockFile.Internal
    ( LockingException(CaughtIOException, UnableToAcquireLockFile)
    , LockingParameters
        ( LockingParameters
        , retryToAcquireLock
        , sleepBetweenRetries
        )
    , RetryStrategy(Indefinitely, No, NumberOfTimes)
    , lock
    , unlock
    )


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
    -> m a
withLockFile params lockFileName action = mask $ \restore -> do
    lockFileHandle <- lock params lockFileName
    r <- restore action `onException` unlock' lockFileHandle
    _ <- unlock' lockFileHandle
    pure r
  where
    unlock' = unlock lockFileName

-- | Type restricted version of 'withLockFile' that discards result of the
-- action.
withLockFile_
    :: (MonadMask m, MonadIO m)
    => LockingParameters
    -> FilePath
    -- ^ Lock file name.
    -> m a
    -> m ()
withLockFile_ = ((void .) .) . withLockFile

-- $usageExample
--
-- Following example acquires lock file and then waits @1000000@ micro seconds
-- before releasing it. Note also that it is possible to specify retry
-- strategy. Here we set it to 'No' and therefore this code won't retry to
-- acquire lock file after first failure.
--
-- @
-- {-\# LANGUAGE TypeApplications \#-}
-- module Main (main)
--   where
--
-- import Control.Concurrent ('Control.Concurrent.threadDelay')
--     -- From base package, but GHC specific.
-- import qualified Control.Exception as Exception ('Control.Exception.handle')
--
-- import Data.Default.Class ('Data.Default.Class.def')
--     -- From data-default-class package, alternatively it's possible to use
--     -- data-default package version 0.5.2 and above.
--     -- <http://hackage.haskell.org/package/data-default-class>
--     -- <http://hackage.haskell.org/package/data-default>
-- import "System.IO.LockFile"
--     ( 'LockingException'
--     , 'LockingParameters'('retryToAcquireLock')
--     , 'RetryStrategy'('No')
--     , 'withLockFile'
--     )
--
--
-- main :: IO ()
-- main = handleException
--     . 'withLockFile' lockParams lockFile $ threadDelay 1000000
--   where
--     lockParams = 'Data.Default.Class.def'
--         { 'retryToAcquireLock' = 'No'
--         }
--
--     lockFile = \"\/var\/run\/lock\/my-example-lock\"
--
--     handleException = Exception.handle
--         $ putStrLn . (\"Locking failed with: \" ++) . show @'LockingException'
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
