-- Test cases for JitCert.Analysis.

module Analysis where

import Control.Monad
import Data.Digest.Pure.SHA
import Test.HUnit

import JitCert.Analysis
import JitCert.Context
import JitCert.Docs
import JitCert.Docs.Shared
import JitCert.GSN.Builder

tests = TestList [testCheckUnboundFigure6, testShadowedFigure5, testCheckUnboundValuesFigure7]

testShadowedFigure5 = TestCase $ do
    (gsn, env) <- runBuilderT $ do
        (t, _, _, _, _, _) <- figure5

        setContextValue t (Test "test_constant" (sha256 "...")) -- "32dfe08c")

    let r = checkShadowedValues gsn env

    -- fail $ show r
    assertError r


testCheckUnboundFigure6 = TestCase $ do

    (gsn, env) <- runBuilderT figureUnboundError

    let x = checkUnbound gsn

    -- fail $ show x
    assertError x

testCheckUnboundValuesFigure7 = TestCase $ do
    (gsn, env) <- runBuilderT $ do
        (fs, rng, _) <- figure7

        let functions = [ (Function "fibonacci" "c9a0ad2c", RNG "rng")
                        , (Function "foo" "f9434869", RNG "rng")]
        setContextValuesWith fs functions $ \(function, generator) -> do
        -- setContextValuesWith fs [RNG] $ \rng -> 

            setContextValue rng generator

            return function

    let r = checkUnboundValues gsn env

    -- fail $ show r
    assertError r



assertError x = when (null x) $ 
    fail "Expected an error"

-- assertLeft (Left _) = return ()
-- assertLeft (Right _) = fail "expected Left"

