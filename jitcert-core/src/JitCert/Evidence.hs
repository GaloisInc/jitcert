module JitCert.Evidence where

import qualified Data.Text.Lazy                as T
import           Data.Time.Clock                ( UTCTime )

-- Evidence types.

type FileName = String

-- data Documented c p = Documented {
--       documentDate :: UTCTime
--     , documentArtifacts :: FileName
--     }
--   deriving (Eq)
-- 
-- -- A hack, should probably have some kind of conditional in `Documented` above
-- instance Evidence Documented where
--   validEvidence Documented{..} _ = True

data Inspected c p = Inspected {
      inspector :: T.Text
    , inspectionDate :: UTCTime
    , inspectionPasses :: Bool
    }
    deriving (Eq)

-- instance Evidence Inspected where
--     validEvidence Inspected{..} _ = inspectionPasses


data StaticProgramAnalysisCertificate c p =
    StaticProgramAnalysisCertificate {
      spCertDate :: UTCTime
    , spCertified :: Bool
    }
    deriving (Eq)


data DynamicProgramAnalysisReport c p =
    DynamicProgramAnalysisReport {
      dpReportDate :: UTCTime
    , dpReport :: FileName
    , dpResult :: Bool
    }

data TestResult c p = TestResult {
      testResultDate :: UTCTime
    , testResult :: Bool
    }
    deriving (Eq)

-- instance Evidence TestResult where
--     validEvidence TestResult{..} _ = testResult

data Report c p = Report {
      reportDate :: UTCTime
    , report :: FileName
    }
    deriving (Eq)
