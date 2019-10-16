module Main where

import System.Exit
import Test.HUnit

import qualified Analysis
import qualified Lookup
import qualified Query

main = do
    let tests = TestList [TestLabel "analysis" Analysis.tests, TestLabel "lookup" Lookup.tests, TestLabel "query" Query.tests]

    results <- runTestTT tests
    if (errors results + failures results == 0) then
      return ()
    else
      exitFailure

