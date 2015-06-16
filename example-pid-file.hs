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
