module JitCert.UTM.Evidence where

import qualified Data.Text.Lazy                as T
import           Data.Time.Clock                ( UTCTime )

-- UTM
data PreflightInspected c = PreflightInspected {
      preflightInspector :: T.Text
    , preflightInspectionDate :: UTCTime
    , preflightInspectionPasses :: Bool
    }
    deriving (Eq)

data FaaRegistration c = FaaRegistration {
      faaRegNumber :: T.Text
    , faaRegDate :: UTCTime
    , faaRegValid :: Bool
    }
    deriving (Eq)
  
data AirplaneWeight c = AirplaneWeight {
      airplaneWeight :: T.Text
    , airplaneWeigthDate :: UTCTime
    , airplaneWeightWithinLimits :: Bool
    }
    deriving (Eq)

data MedicalChecklist c = MedicalChecklist {
      medicalChecklist :: T.Text
    , medicalChecklistInspected :: UTCTime
    , medicalChecklistPassed :: Bool
    }
    deriving (Eq)
  
data UasCertificate c = UasCertificate {
      uasCertificateNumber :: T.Text
    , uasCertificateIssuedOn :: UTCTime
    , uasCertificateValid :: Bool
    }
    deriving (Eq)

data Assesment c p = Assesment {
      assesmentInspector :: T.Text
    , assesmentDate :: UTCTime
    , assesmentPasses :: Bool
    }
    deriving (Eq)

data Briefing c = Briefing {
      briefing :: T.Text
    , briefingTime :: UTCTime
    , briefingNotamsPosted :: Bool
    }
    deriving (Eq)

data AuthorizationRequest c = AuthorizationRequest {
      requestLocation :: T.Text
    , requestType :: T.Text -- LAANC, UTM, COA
    , requestTime :: UTCTime
    , requestApproved :: Bool
    }
    deriving (Eq)

data AeronaticalChart c = AeronaticalChart {
      chartVersion :: T.Text
    , chartAirspaceUncontrolled :: Bool
    }
    deriving (Eq)

data UserManual c = UserManual {
      userManualVersion :: T.Text
    , userManualMaxWind :: Int
    }
    deriving (Eq)

data WeatherForecastEvidence c = WeatherForecastEvidence {
      weatherForecast :: T.Text
    , weatherForecastTime :: UTCTime
    }
    deriving (Eq)

data VisualObservationEvidence c = VisualObservationEvidence {
      visualObservers :: T.Text
    }
    deriving (Eq)

