module Lookup where

import qualified Data.List as List
import Data.Time.Clock
import Test.HUnit

import JitCert.Docs
import JitCert.GSN
import JitCert.GSN.Builder
import JitCert.Query

tests = TestList [testContextLookup, testSolutionLookup]

testContextLookup = TestCase $ do
    ((_, fs, f, _, _, funcs'), gsn, env) <- runBuilderT' figure5
    let funcs = map (\(x, _, _) -> x) funcs'

    let fsSC = lookupContextNodeValues (nodeId fs) gsn env
    assertEqual "env lookup for fs not equal" [SomeContext funcs] fsSC 

    let fSC = lookupContextNodeValues (nodeId f) gsn env
    assertBool "env lookup for f not equal" $ listEq (map SomeContext funcs) fSC
    
    
-- Equality ignoring order without Ord.
listEq :: Eq a => [a] -> [a] -> Bool
listEq as bs = List.length as == List.length bs && all (`List.elem` as) bs

testSolutionLookup = TestCase $ do
    ((_, _, _, _, s, vs'), _, env) <- runBuilderT' figure5
    let vs = map (\(_, _, x) -> x) vs'

    let ss =  lookupSolutionNodeValues (nodeId s) env

    assertBool "env lookup for ss not equal" $ listEq (map SomeEvidence vs) ss


