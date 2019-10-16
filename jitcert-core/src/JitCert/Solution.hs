module JitCert.Solution where

import qualified Data.Text.Lazy as T
import           Data.Time.Clock
import           Data.Time.Format
import           JitCert.Context
import           JitCert.GSN.Types
import           JitCert.Evidence
import           Type.Reflection

timeToText :: UTCTime -> T.Text
timeToText = T.pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing)

-- data Documents c p = Documents (Node Document Context) (Node c Context)

data Tests c p = Tests (Node Test Context) (Node c Context) p
instance (Typeable c, Typeable p, RenderContext c, Property p) => SolutionType (Tests c p) where
    type SolutionEvidence (Tests c p) = TestResult c p

    validEvidence TestResult {..} = testResult

    renderEvidence TestResult{..} = "Tests run on " <> timeToText testResultDate <> if testResult then " passed" else " failed"

    renderSolution (Tests t c p) =
        [SFContext t, SFText "that", SFContext c, SFProperty p]


data ManualInspection c p = ManualInspection (Node c Context) p
instance (Typeable c, Typeable p, RenderContext c, Property p) => SolutionType (ManualInspection c p) where
    type SolutionEvidence (ManualInspection c p) = Inspected c p

    validEvidence Inspected {..} = inspectionPasses

    renderEvidence Inspected{..} = "Inspection by " <> inspector <> " on " <> timeToText inspectionDate <> if inspectionPasses then " passed" else " failed"

    renderSolution (ManualInspection c p) =
        [SFText "Manual inspection that", SFContext c, SFProperty p]

data StaticProgramAnalysis c p = StaticProgramAnalysis (Node c Context) p

instance (Typeable c, Typeable p, RenderContext c, Property p) => SolutionType (StaticProgramAnalysis c p) where
    type SolutionEvidence (StaticProgramAnalysis c p)
        = StaticProgramAnalysisCertificate c p

    validEvidence StaticProgramAnalysisCertificate {..} = spCertified

    renderEvidence StaticProgramAnalysisCertificate{..} = "Static program analysis run on " <> timeToText spCertDate <> if spCertified then " passed" else " failed"

    renderSolution (StaticProgramAnalysis c p) =
        [SFText "Static program analysis that", SFContext c, SFProperty p]


data DynamicProgramAnalysis c p = DynamicProgramAnalysis (Node c Context) p

instance (Typeable c, Typeable p, RenderContext c, Property p) => SolutionType (DynamicProgramAnalysis c p) where
    type SolutionEvidence (DynamicProgramAnalysis c p)
        = DynamicProgramAnalysisReport c p

    validEvidence DynamicProgramAnalysisReport {..} = dpResult

    renderEvidence DynamicProgramAnalysisReport{..} = "Dynamic program analysis run on " <> timeToText dpReportDate <> if dpResult then " passed" else " failed"

    renderSolution (DynamicProgramAnalysis c p) =
        [SFText "Dynamic program analysis that", SFContext c, SFProperty p]
