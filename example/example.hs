{-# LANGUAGE TypeApplications #-}
-- |
-- Module:       Main
-- Description:  Simple example that acquires lock for a short period of time.
-- Copyright:    (c) 2013, 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
module Main (main)
  where

import Control.Concurrent (threadDelay)
    -- From base package, but GHC specific.
import qualified Control.Exception as Exception (handle)

import Data.Default.Class (Default(def))
    -- From data-default-class package, alternatively it's possible to use
    -- data-default package version 0.5.2 and above.
    -- http://hackage.haskell.org/package/data-default-class
    -- http://hackage.haskell.org/package/data-default
import System.IO.LockFile
    ( LockingException
    , LockingParameters(retryToAcquireLock)
    , RetryStrategy(No)
    , withLockFile
    )


main :: IO ()
main = handleException
    . withLockFile lockParams lockFile $ threadDelay 1000000
  where
    lockParams = def
        { retryToAcquireLock = No
        }

    lockFile = "/var/run/lock/my-example-lock"

    handleException = Exception.handle
        $ putStrLn . ("Locking failed with: " ++) . show @LockingException
