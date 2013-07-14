-- |
-- Module:       $HEADER$
-- Description:  Tests for module System.IO.LockFile.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
--
-- Tests for module @System.IO.LockFile@.
module TestCase.System.IO.LockFile (tests)
    where

import Control.Concurrent (threadDelay)

import qualified Control.Monad.TaggedException.Hidden as E (hide)
import Data.Default.Class (Default(def))
import System.Directory (doesFileExist)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (assertBool)

import System.IO.LockFile


tests :: [Test]
tests =
    [ testGroup "withLockExt"
        test_withLockExt
    , testGroup "withLockFile"
        [ test_lockFileIsPresent
        , test_lockFileIsDeletedAfterwards
        ]
    ]

test_withLockExt :: [Test]
test_withLockExt =
    [ testProperty "File names are generated correctly" $ \ s ->
        withLockExt s == s ++ ".lock"
    ]
{-# ANN test_withLockExt "HLint: ignore Use camelCase" #-}

test_lockFileIsPresent :: Test
test_lockFileIsPresent =
    testCase "Lock file is present while running computation"
        $ E.hide (withLockFile def lockFileName (doesFileExist lockFileName))
            >>= assertBool failureMsg
  where
    lockFileName = withLockExt "./test/test-lock-file"
    failureMsg = "Function withLockFile failed acquire lock file "
        ++ show lockFileName
{-# ANN test_lockFileIsPresent "HLint: ignore Use camelCase" #-}

test_lockFileIsDeletedAfterwards :: Test
test_lockFileIsDeletedAfterwards =
    testCase "Lock file is deleted afterwards" $ do
        E.hide . withLockFile def lockFileName $ threadDelay 1000
        doesFileExist lockFileName >>= assertBool failureMsg . not
  where
    lockFileName = withLockExt "./test/test-lock-file"
    failureMsg = "Function withLockFile failed to delete lock file "
        ++ show lockFileName
{-# ANN test_lockFileIsDeletedAfterwards "HLint: ignore Use camelCase" #-}
