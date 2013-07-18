-- |
-- Module:       Main
-- Description:  Simple example that acquires lock for a short period of time.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
module Main (main)
    where

import Control.Concurrent (threadDelay)

import qualified Control.Monad.TaggedException as Exception (handle)
import Data.Default.Class (Default(def))
import System.IO.LockFile
    ( LockingParameters(retryToAcquireLock)
    , RetryStrategy(No)
    , withLockFile
    )


main :: IO ()
main = Exception.handle (putStrLn . ("Locking failed with: " ++) . show)
    . withLockFile def{retryToAcquireLock = No} "/var/run/lock/my-example-lock"
    $ threadDelay 1000000
