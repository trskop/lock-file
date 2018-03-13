{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Tests for module System.IO.LockFile.
-- Copyright:    (c) 2013-2015, 2018 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
--
-- Tests for module @System.IO.LockFile@.
module TestCase.System.IO.LockFile (tests)
    where

import Control.Applicative (pure)
import Control.Concurrent (threadDelay)
import qualified Control.Exception as Exception (catch)
import Control.Monad ((>>=))
import Data.Bool (Bool(False, True), not)
import Data.Eq ((==))
import Data.Function ((.), ($))
import Data.Functor ((<$))
import Data.List ((++))
import System.IO (FilePath)
import Text.Show (show)

import Data.Default.Class (Default(def))
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (assertBool)

import System.IO.LockFile
  ( LockingException(CaughtIOException)
  , withLockExt
  , withLockFile
  )


tests :: [Test]
tests =
    [ testGroup "withLockExt"
        test_withLockExt
    , testGroup "withLockFile"
        [ test_lockFileIsPresent
        , test_lockFileIsDeletedAfterwards
        , test_lockingFailedDueToNonExistingDirectory
        ]
    ]

test_withLockExt :: [Test]
test_withLockExt =
    [ testProperty "File names are generated correctly" $ \ s ->
        withLockExt s == s ++ ".lock"
    ]
{-# ANN test_withLockExt "HLint: ignore Use camelCase" #-}

lockFileName :: FilePath
lockFileName = withLockExt $ "test" </> "test-lock-file"

test_lockFileIsPresent :: Test
test_lockFileIsPresent =
    testCase "Lock file is present while running computation"
        $ theTest >>= assertBool failureMsg
  where
    theTest = withLockFile def lockFileName (doesFileExist lockFileName)
    failureMsg = "Function withLockFile failed acquire lock file "
        ++ show lockFileName
{-# ANN test_lockFileIsPresent "HLint: ignore Use camelCase" #-}

test_lockFileIsDeletedAfterwards :: Test
test_lockFileIsDeletedAfterwards =
    testCase "Lock file is deleted afterwards" $ do
        withLockFile def lockFileName $ threadDelay 1000
        doesFileExist lockFileName >>= assertBool failureMsg . not
  where
    failureMsg = "Function withLockFile failed to delete lock file "
        ++ show lockFileName
{-# ANN test_lockFileIsDeletedAfterwards "HLint: ignore Use camelCase" #-}

test_lockingFailedDueToNonExistingDirectory :: Test
test_lockingFailedDueToNonExistingDirectory =
    testCase "Locking failed due to non existing directory"
        $ (withLockFile def lockFileName' (False <$ threadDelay 1000)
            `Exception.catch` handler) >>= assertBool failureMsg
  where
    lockFileName' = "this-directory-does-not-exist" </> lockFileName
    failureMsg = "Function withLockFile should fail when creating lock in non"
        ++ " existing directory: " ++ show lockFileName'
    handler = pure . \case
        CaughtIOException _ -> True
        _ -> False
{-# ANN test_lockingFailedDueToNonExistingDirectory
    "HLint: ignore Use camelCase" #-}
