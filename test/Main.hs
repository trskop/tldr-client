-- |
-- Module:      Main
-- Description: Unit tests for the tldr-client package
-- Copyright:   (c) 2024 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Unit tests for the @tldr-client@ package.
module Main
    ( main
    , )
  where

import System.IO (IO, )

import Test.Tasty (TestTree, defaultMain, testGroup, )

import Tests.TldrClient.Locale qualified (tests, )


main :: IO ()
main = defaultMain (testGroup "Tests" tests)

tests :: [TestTree]
tests =
    [ testGroup "Tests.TldrClient.Locale" Tests.TldrClient.Locale.tests
    ]
