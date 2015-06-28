Lock File
=========

[![Hackage](http://img.shields.io/hackage/v/lock-file.svg)][Hackage: lock-file]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/lock-file.svg)](http://packdeps.haskellers.com/reverse/lock-file)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]

[![Build](https://travis-ci.org/trskop/lock-file.svg)](https://travis-ci.org/trskop/lock-file)


Description
-----------

Provide exclusive access to a resource using lock file, which are files whose
purpose is to signal by their presence that some resource is locked.


Usage Example
-------------

Following example acquires lock file and then waits `1000000` micro seconds
before releasing it. Note also that it is possible to specify retry strategy.
Here we set it to `No` and therefore this code won't retry to acquire lock file
after first failure.

```Haskell
module Main (main)
    where

import Control.Concurrent (threadDelay)
    -- From base package, but GHC specific.

import qualified Control.Monad.TaggedException as Exception (handle)
    -- From tagged-exception-core package.
    -- http://hackage.haskell.org/package/tagged-exception-core
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
main = handleException
    . withLockFile lockParams lockFile $ threadDelay 1000000
  where
    lockParams = def
        { retryToAcquireLock = No
        }

    lockFile = "/var/run/lock/my-example-lock"

    handleException = Exception.handle
        $ putStrLn . ("Locking failed with: " ++) . show
```

This command line example shows that trying to execute two instances of
`example` at the same time will result in failure of the second one.

```
$ ghc example.hs
[1 of 1] Compiling Main             ( example.hs, example.o )
Linking example ...
$ ./example & ./example
[1] 7893
Locking failed with: Unable to acquire lock file: "/var/run/lock/my-example-lock"
$ [1]+  Done                    ./example
```

PID File Example
----------------

Lock file, as implemented by this library, is created containing PID of the
process that created it. PID file, on UNIX-like system, is a special case of
lock file. It prevents the same daemon to be started up multiple times.

Thanks to the above fact we can now create function with following type
signature:

```Haskell
withPidFile :: IO () -> IO ()
```

Its purpose is to wrap application `main` and acquire PID file prior to passing
control to the application code, or fail if PID file is already acquired.

```Haskell
{-# LANGUAGE NoImplicitPrelude #-}
module Main
  where

import Control.Monad (Monad(return), (>=>))
import Control.Concurrent (threadDelay)
import Data.Function ((.), ($), const)
import Data.Functor ((<$>))
import Data.List ((++))
import System.Environment (getEnv, getProgName)
import System.Exit (exitFailure)
import System.IO (IO, FilePath, hPutStrLn, putStrLn, stderr)
import Text.Show (show, showString)

import System.Posix.User (getEffectiveUserID)

import Control.Monad.TaggedException (catch)
import Data.Default.Class (Default(def))
import System.IO.LockFile
    ( LockingException(CaughtIOException, UnableToAcquireLockFile)
    , LockingParameters(retryToAcquireLock)
    , RetryStrategy(No)
    , withLockFile
    )


withPidFile :: IO () -> IO ()
withPidFile m = do
    pidFilePath <- mkPidFilePath
    withLockFile def{retryToAcquireLock = No} pidFilePath m
        `catch` (printLockingException pidFilePath >=> const exitFailure)
  where
    mkPidFilePath :: IO FilePath
    mkPidFilePath = do
        fileName <- (++ ".pid") <$> getProgName
        euid <- getEffectiveUserID
        case euid of
            0 -> return $ "/var/run/" ++ fileName
            _ -> (++ ('/' : '.' : fileName)) <$> getEnv "HOME"
                -- This may throw exception if $HOME environment varialbe is
                -- not set.

    printLockingException :: FilePath -> LockingException -> IO ()
    printLockingException filePath exception = hPutStrLn stderr errorMsg
      where
        errorMsg = case exception of
            UnableToAcquireLockFile _ -> mkMsg "File already exists."
            CaughtIOException       e -> mkMsg $ show e

        mkMsg = showString filePath . showString ": Unable to create PID file: "

main :: IO ()
main = withPidFile $ do
    putStrLn "Hello World!"
    threadDelay 1000000
```


Building options
----------------

* `-fpedantic` (disabled by default)

  Pass additional warning flags to GHC.



[Hackage: lock-file]:
  https://hackage.haskell.org/package/lock-file
  "Hackage: lock-file"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"
