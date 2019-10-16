-- Test cases for JitCert.Query.

module Query where

import Data.Digest.Pure.SHA
import qualified Data.Set as Set
import Test.HUnit

import JitCert.Context
import JitCert.Docs
-- import JitCert.DotBackend
import JitCert.GSN.Builder
import JitCert.GSN.Types
import JitCert.Query

tests = TestList [testEnvDiffFigure5, testFindManualInspectionOrStaticAnalysis]

testEnvDiffFigure5 = TestCase $ do
    (gsn, env0) <- runBuilderT $ do
        (_, _, _, t, _) <- figure5'

        setContextValue t $ Test "test0" (sha256 "dc88f819")

    (_, env1) <- runBuilderT $ do
        (_, _, _, t, _) <- figure5'

        setContextValue t $ Test "test1" (sha256 "a7103356")

    let nodes = envDiff gsn env0 env1
    let nIds = Set.map _sNodeId nodes

    -- fail $ show $ map (snd . someNodeToLNode) $ Set.toList nodes

    assertEqual "Only nodes 4 and 6 should be dirty" nIds $ Set.fromList [4,6]


testFindManualInspectionOrStaticAnalysis = TestCase $ do
    (gsn, env) <- runBuilderT figure8

    let nodes = findManualInspectionOrStaticAnalysis gsn
    let nIds = Set.map _sNodeId $ Set.fromList nodes

    assertEqual "Found wrong nodes when looking for manual inspection or static analysis" nIds $ Set.fromList [1]



