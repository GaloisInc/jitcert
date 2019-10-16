module JitCert.Docs.FunctionsCorrect where

import           Data.Digest.Pure.SHA

import           JitCert.Context
import           JitCert.DocGenerator
import           JitCert.Docs.Shared
import           JitCert.Evidence
import           JitCert.GSN.Builder
import           JitCert.GSN.Types
import           JitCert.Properties
import           JitCert.Solution

figure5'
    :: Monad m
    => BuilderT
           m
           ( Node [Function] 'Context
           , Node Function 'Context
           , Node Function 'Goal
           , Node Test 'Context
           , Node (Tests Function IsCorrect) 'Solution
           )
figure5' = do
    fs     <- context @[Function] "fs" Nothing
    (f, p) <- forall "f" fs

    gs     <- goalWithOptions p [SFContext f, SFText "is correct"] fsOptions
    addEdge gs fs

    t <- context @Test "t" Nothing
    addEdge gs t

    s <- solution (Tests t f IsCorrect)
    addEdge gs s

    return (fs, f, gs, t, s)

fsOptions :: NodeOptions
fsOptions = setNodeOptionsGenerator def $ dgText "Functions are correct."

figure5Functions :: [(Function, Test, TestResult c p)]
figure5Functions =
    [ ( Function "fibonacci" "c9a0ad2c"
      , Test "test_fibonacci" (sha256 "...")
      , TestResult (read "2000-01-01 00:00:00.000000 UTC") True
      )
    , ( Function "foo" "f9434869"
      , Test "test_foo" (sha256 "...")
      , TestResult (read "2000-01-01 00:00:00.000000 UTC") False
      )
    ]

figure5
    :: (Monad m)
    => BuilderT
           m
           ( Node Test Context
           , Node [Function] Context
           , Node Function Context
           , Node Function 'Goal
           , Node (Tests Function IsCorrect) Solution
           , [(Function, Test, TestResult Function IsCorrect)]
           )
figure5 = do
    (fs, f, gs, t, s) <- figure5'

    let values = figure5Functions
    setContextValuesWith fs values
        $ \(function, functionTest, functionTestResult) -> do
              setContextValue t functionTest
              setSolutionEvidence s functionTestResult

              return function

    return (t, fs, f, gs, s, values)
