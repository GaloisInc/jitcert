module Main where

import           Control.Monad
import           Data.Monoid ( (<>) )
import qualified Data.Set as Set
import qualified Data.Text.Lazy.IO as T
import           Data.Time.Clock
import           System.IO ( hPutStrLn, stderr )

import           JitCert.Analysis
import qualified JitCert.Context as Context
import           JitCert.DotBackend ( renderPrintWithEnvEval )
import           JitCert.Evidence ( Inspected(..) )
import           JitCert.FIPS.Combinators
import           JitCert.GSN ( Node(..), NodeTy(..), SentenceFragment(..), NodePolicy(..))
import qualified JitCert.GSN as GSN
import           JitCert.GSN.Builder
import           JitCert.GSN.Combinators
import qualified JitCert.GSNAnalysis as GA
import           JitCert.Internal
import           JitCert.Query
import           JitCert.Solution

import           FIPS.Context
import           FIPS.Property

type FNode n = GSN.Node n

main :: IO ()
main = do
    -- Get contexts.
    openssl <- Software "OpenSSL" "1.0"
        <$> Context.loadFiles ["fips/Main.hs"]

    prng <- Software "PRNG" "1.0"
        <$> Context.loadFiles ["fips/Main.hs"]

    -- Get evidences.
    now <- getCurrentTime
    let insp        = Inspected "Alice" now True

    let components' = [(prng, FIPS.Context.LogicalBoundary, insp), (openssl, FIPS.Context.LogicalBoundary, insp)]

    (gsn, env) <- runBuilderT $ buildFIPSBoundaryWith components'

    let es = runChecks gsn env
    unless (Set.null es) $ --do
        mapM_ (T.hPutStrLn stderr . gsnErrorToMessage) es
        --exitFailure


    --TODO: Merge this check with runChecks

    whenLeft (GA.visitNodes gsn) $ \err -> do
        hPutStrLn stderr "\n============ Assurance case failed to pass checks ============"
        hPutStrLn stderr err
        --exitFailure



    let nodes = findManualInspectionOrStaticAnalysis gsn

    hPutStrLn stderr
        $  "Found nodes: "
        <> (show $ map someNodeIdToNodeDescription nodes)

    --hPutStrLn stderr "\n============ Evaluation Result ============"
    renderPrintWithEnvEval (GA.evaluateArgument gsn env) gsn env
    --hPutStrLn stderr $ GA.prettyPrintEvaluation gsn env

  where
    whenLeft (Left e) f = f e
    whenLeft _        _ = return ()

-- buildFIPSApproved :: Builder ()
-- buildFIPSApproved = do
--     cCryptographicAlgorithm <- context @CryptographicAlgorithm "c" Nothing -- "Cryptographic algorithm, c"
-- 
--     validApprovedAlgorithm <- goal PolicyAnd [SFContext cCryptographicAlgorithm, SFText "is valid and approved"]
--     addContext validApprovedAlgorithm cCryptographicAlgorithm
-- 
--     cApprovedAlgorithms <- context @[CryptographicAlgorithm] "cs" $ Just "List of approved cryptographic algorithms"
--     
--     approvedAlgorithm <- goal PolicyAnd [SFContext cCryptographicAlgorithm, SFText "is approved"]
--     addEdge validApprovedAlgorithm approvedAlgorithm
--     addContext approvedAlgorithm cApprovedAlgorithms
-- 
--     sApprovedAlgorithm <- solution [SFContext cCryptographicAlgorithm, SFText "is in FIPS approved", SFContext cApprovedAlgorithms]
--     addEdge approvedAlgorithm sApprovedAlgorithm
-- 
-- 
--     -- setChildren approvedAlgorithm [sApprovedAlgorithm]
-- 
--     -- setChildren validApprovedAlgorithm [approvedAlgorithm, sApprovedAlgorithm]
--     -- setChildren validApprovedAlgorithm $ 
--     --  ... &&.  ...
--     
-- 
--     validAlgorithm <- goal PolicyAnd [SFContext cCryptographicAlgorithm, SFText "is valid"]
--     addEdge validApprovedAlgorithm validAlgorithm
--     
--     return ()

-- buildFIPSBoundaryWith :: [Component] -> LogicalBoundary -> Inspected Component NoIOExceptRNG -> BuilderT IO ()
buildFIPSBoundaryWith
    :: [(Component, LogicalBoundary, Inspected Component NoIOExceptRNG)]
    -> BuilderT IO ()
buildFIPSBoundaryWith components = do
    (cCryptographicBoundaryComponents, cCryptographicBoundaryLogical, snCryptographicBoundaryLogicalProtectedNoIOManual) <-
        buildFIPSBoundary

    -- Set context values for each component.
    setContextValuesWith cCryptographicBoundaryComponents components
        $ \(component, logBoundary, insp) -> do

        -- Set context value for current component.
              setContextValue cCryptographicBoundaryLogical logBoundary

              -- Set evidence values.
              setSolutionEvidence
                  snCryptographicBoundaryLogicalProtectedNoIOManual
                  insp

              return component

    -- Uncomment to demonstrate value shadowing.
    -- setContextValue cCryptographicBoundaryLogical LogicalBoundary


buildFIPSBoundary
    :: BuilderT
           IO
           ( Node [Component] Context
           , Node LogicalBoundary Context
           , Node (ManualInspection Component NoIOExceptRNG) Solution
           )
buildFIPSBoundary = do
    -- Build GSN structure.

    gUndeveloped           <- goal PolicyAnd [SFText "Undeveloped"]

    gCryptographicBoundary <- goal
        PolicyAnd
        [ SFText
              "The cryptographic module is contained within the cryptographic boundary"
        ]

    sCryptographicBoundary <- strategy
        PolicyAnd
        [ SFText
              "Argument by ensuring all components of a cryptographic module are inside the logical and physical cryptographic boundary"
        ]
    addEdge gCryptographicBoundary sCryptographicBoundary

    cCryptographicBoundaryComponents <-
        context @[Component] "cs"
            $ Just
                  "List of all components of the cryptographic module (hardware, software, or firmware that implements approved security functions)"-- $ List CryptographicBoundaryComponent 
    --addContext sCryptographicBoundary cCryptographicBoundaryComponents

    (cComponent, pForallComponent) <- forall
        "c"
        cCryptographicBoundaryComponents
    gCryptographicBoundaryForall <- goal
        pForallComponent
        [ SFContext cComponent
        , SFText "is contained within the cryptographic boundary"
        ] -- Do we need a lambda here?Take Component of list as argument?
    addEdge sCryptographicBoundary gCryptographicBoundaryForall

    addContext gCryptographicBoundaryForall cCryptographicBoundaryComponents


    cCryptographicBoundaryLogicalWhether <- contextVerb
        "is software/firmware"
        -- [SFText "Whether ", SFContext cComponent, " is software/firmware"]
        -- cComponent
        isSoftwareFirmware

    gCryptographicBoundaryLogical <- goal
        (PolicyWhen cComponent cCryptographicBoundaryLogicalWhether PolicyAnd)
        [ SFContext cComponent
        , SFText "is contained within the logical cryptographic boundary"
        ]
    addEdge gCryptographicBoundaryForall gCryptographicBoundaryLogical
    addContext gCryptographicBoundaryLogical
               cCryptographicBoundaryLogicalWhether

    cCryptographicBoundaryLogical <-
        context @LogicalBoundary "p"
            $ Just
                  "The processes, threads, and memory that run cryptographic functionality"
    addEdge gCryptographicBoundaryLogical cCryptographicBoundaryLogical

    -- Switch to this version to demonstrate bound error.
    -- gCryptographicBoundaryPhysical <- goal PolicyAnd [SFContext cComponent, SFText "is contained within the physical cryptographic boundary", SFContext cCryptographicBoundaryLogical]
    gCryptographicBoundaryPhysical <- goal
        PolicyAnd
        [ SFContext cComponent
        , SFText "is contained within the physical cryptographic boundary"
        ]
    addEdge gCryptographicBoundaryForall   gCryptographicBoundaryPhysical
    addEdge gCryptographicBoundaryPhysical gUndeveloped


    gCryptographicBoundaryLogicalRun <- goal
        PolicyAnd
        [ SFContext cComponent
        , SFText
            "is run on the processes, threads, and memory defined in the cryptographic boundary"
        ]
    -- JP: There's slightly different phrasing here. Can we normalize these?
    addEdge gCryptographicBoundaryLogical gCryptographicBoundaryLogicalRun

    snCryptographicBoundaryLogicalRun <- manualInspection cComponent
        $ RunOnBoundary cCryptographicBoundaryLogical
    -- solution [SFText "Manual code inspection that", SFContext cComponent, SFText "is run on", SFContext cCryptographicBoundaryLogical] -- processes, threads, and memory defined within the cryptographic boundary"
    addEdge gCryptographicBoundaryLogicalRun snCryptographicBoundaryLogicalRun

    gCryptographicBoundaryLogicalProtected <- goal
        PolicyAnd
        [ SFContext cCryptographicBoundaryLogical
        , SFText "(process, threads, memory) of"
        , SFContext cComponent
        , SFText "is isolated and protected from outside influence"
        ]
    addEdge gCryptographicBoundaryLogical gCryptographicBoundaryLogicalProtected
    addEdge gCryptographicBoundaryLogicalProtected gUndeveloped

    gCryptographicBoundaryLogicalProtectedNoSideChannels <- goal
        PolicyAnd
        [SFContext cComponent, SFText "is free from side channels"]
    addEdge gCryptographicBoundaryLogicalProtected
            gCryptographicBoundaryLogicalProtectedNoSideChannels

    snCryptographicBoundaryLogicalProtectedNoSideChannelsAnalysis <-
        staticProgramAnalysis cComponent NoSideChannels
    -- solution [SFText "Static analysis that", SFContext cComponent, SFText "is free from side channels"]
    addEdge gCryptographicBoundaryLogicalProtectedNoSideChannels
            snCryptographicBoundaryLogicalProtectedNoSideChannelsAnalysis


    (gCryptographicBoundaryLogicalProtectedNoIO, snCryptographicBoundaryLogicalProtectedNoIOManual, _) <-
        manualInspectionOrStaticAnalysis
            [SFContext cComponent, SFProperty NoIOExceptRNG]
            cComponent
            NoIOExceptRNG
    addEdge gCryptographicBoundaryLogicalProtected
            gCryptographicBoundaryLogicalProtectedNoIO

    let pNoShared = NoSharedMemoryOrState cCryptographicBoundaryLogical
    (gCryptographicBoundaryLogicalProtectedThread, _, _) <-
        manualInspectionOrStaticAnalysis
            [ SFText "threads of"
            , SFContext cComponent
            , SFText "in"
            , SFContext cCryptographicBoundaryLogical
            , SFProperty pNoShared
            ]
            cComponent
            pNoShared
    addEdge gCryptographicBoundaryLogicalProtected
            gCryptographicBoundaryLogicalProtectedThread

    return
        ( cCryptographicBoundaryComponents
        , cCryptographicBoundaryLogical
        , snCryptographicBoundaryLogicalProtectedNoIOManual
        )
