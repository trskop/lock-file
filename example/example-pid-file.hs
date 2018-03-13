{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main
  where

import Control.Applicative (pure)
import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Control.Monad ((>=>), (>>=))
import Data.Function ((.), ($), const)
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import System.Environment (getEnv, getProgName)
import System.Exit (exitFailure)
import System.IO (IO, FilePath, hPutStrLn, putStrLn, stderr)
import Text.Show (show, showString)

import System.Posix.User (getEffectiveUserID)

import Data.Default.Class (def)
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
        fileName <- (<> ".pid") <$> getProgName
        getEffectiveUserID >>= \case
            0 -> pure $ "/var/run/" <> fileName
            _ -> (<> ('/' : '.' : fileName)) <$> getEnv "HOME"
                -- This may throw exception if $HOME environment varialbe is
                -- not set.

    printLockingException :: FilePath -> LockingException -> IO ()
    printLockingException filePath = hPutStrLn stderr . mkMsg . \case
        UnableToAcquireLockFile _ -> "File already exists."
        CaughtIOException       e -> show e
      where
        mkMsg =
            showString filePath . showString ": Unable to create PID file: "

main :: IO ()
main = withPidFile $ do
    putStrLn "Hello World!"
    threadDelay 1000000
