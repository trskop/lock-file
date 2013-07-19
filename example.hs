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
    -- From base package, but GHC specific.

import qualified Control.Monad.TaggedException as Exception (handle)
    -- From tagged-exception-core package.
    -- https://github.com/trskop/tagged-exception
import Data.Default.Class (Default(def))
    -- From data-default-class package, alternatively it's possible to use
    -- data-default package version 0.5.2 and above.
    -- http://hackage.haskell.org/package/data-default-class
    -- http://hackage.haskell.org/package/data-default
import System.IO.LockFile
    ( LockingParameters(retryToAcquireLock)
    , RetryStrategy(No)
    , withLockFile
    )


main :: IO ()
main = Exception.handle (putStrLn . ("Locking failed with: " ++) . show)
    . withLockFile def{retryToAcquireLock = No} "/var/run/lock/my-example-lock"
    $ threadDelay 1000000
