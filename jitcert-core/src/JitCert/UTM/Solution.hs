module JitCert.UTM.Solution where

import qualified Data.Text.Lazy as T
import           Data.Time.Clock
import           Data.Time.Format
import           JitCert.GSN.Types
import           JitCert.UTM.Evidence
import           Type.Reflection

timeToText :: UTCTime -> T.Text
timeToText = T.pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing)


-- UTM
-- PreflightChecks
newtype PreflightChecks c = PreflightChecks (Node c Context)
instance (Typeable c, RenderContext c) => SolutionType (PreflightChecks c) where
    type SolutionEvidence (PreflightChecks c) = PreflightInspected c

    validEvidence PreflightInspected{..} = preflightInspectionPasses

    renderEvidence PreflightInspected{..} = "Inspection by " <> preflightInspector <> " on " <> timeToText preflightInspectionDate <> if preflightInspectionPasses then " passed" else " failed"

    renderSolution (PreflightChecks c) =
        [SFText "Preflight checks,", SFContext c]

-- FaaRegistration
newtype ValidFaaRegistration c = ValidFaaRegistration (Node c Context)
instance (Typeable c, RenderContext c) => SolutionType (ValidFaaRegistration c) where
    type SolutionEvidence (ValidFaaRegistration c) = FaaRegistration c

    validEvidence FaaRegistration{..} = faaRegValid

    renderEvidence FaaRegistration{..} = "Registration number " <> faaRegNumber <> " registred " <> timeToText faaRegDate <> if faaRegValid then " valid" else " invalid"

    renderSolution (ValidFaaRegistration c) =
        [SFText "FAA Registration number, ", SFContext c]

-- CheckAirplaneWeight
newtype CheckAirplaneWeight c = CheckAirplaneWeight (Node c Context)
instance (Typeable c, RenderContext c) => SolutionType (CheckAirplaneWeight c) where
    type SolutionEvidence (CheckAirplaneWeight c) = AirplaneWeight c

    validEvidence AirplaneWeight{..} = airplaneWeightWithinLimits

    renderEvidence AirplaneWeight{..} = "Airplane weight " <> airplaneWeight <> " measured " <> timeToText airplaneWeigthDate <> if airplaneWeightWithinLimits then " within limits" else " not within limits"

    renderSolution (CheckAirplaneWeight c) =
        [SFText "Airplane weight", SFContext c]

-- CheckMedicalCondition
newtype CheckMedicalCondition c = CheckMedicalCondition (Node c Context)
instance (Typeable c, RenderContext c) => SolutionType (CheckMedicalCondition c) where
    type SolutionEvidence (CheckMedicalCondition c) = MedicalChecklist c

    validEvidence MedicalChecklist{..} = medicalChecklistPassed

    renderEvidence MedicalChecklist{..} = "Following medical contions checklist " <> medicalChecklist <> " was checked " <> timeToText medicalChecklistInspected <> if medicalChecklistPassed then " passed" else " failed"

    renderSolution (CheckMedicalCondition c) =
        [SFText "Medical condition checklist, ", SFContext c]

-- ValidUasCertificate
newtype ValidUasCertificate c = ValidUasCertificate (Node c Context)
instance (Typeable c, RenderContext c) => SolutionType (ValidUasCertificate c) where
    type SolutionEvidence (ValidUasCertificate c) = UasCertificate c

    validEvidence UasCertificate{..} = uasCertificateValid

    renderEvidence UasCertificate{..} = "UAS Certificate " <> uasCertificateNumber <> " issued " <> timeToText uasCertificateIssuedOn <> if uasCertificateValid then " valid" else " invalid"

    renderSolution (ValidUasCertificate c) =
        [SFText "UAS Certificate, ", SFContext c]

-- SafetyRiskAssesment
data SafetyRiskAssesment c p = SafetyRiskAssesment (Node c Context) p
instance (Typeable c, Typeable p, RenderContext c, Property p) => SolutionType (SafetyRiskAssesment c p) where
    type SolutionEvidence (SafetyRiskAssesment c p) = Assesment c p

    validEvidence Assesment{..} = assesmentPasses

    renderEvidence Assesment{..} = "Safety Risk Assesment by " <> assesmentInspector <> " on " <> timeToText assesmentDate <> if assesmentPasses then " passed" else " failed"

    renderSolution (SafetyRiskAssesment c p) =
        [SFText "Safety Risk Assesment: ", SFContext c, SFProperty p]

-- CheckPreflightBriefing
newtype CheckPreflightBriefing c = CheckPreflightBriefing (Node c Context)
instance (Typeable c, RenderContext c) => SolutionType (CheckPreflightBriefing c) where
    type SolutionEvidence (CheckPreflightBriefing c) = Briefing c

    validEvidence Briefing{..} = not briefingNotamsPosted

    renderEvidence Briefing{..} = "Preflight briefing " <> briefing <> " from " <> timeToText briefingTime <> if not briefingNotamsPosted then " clear" else " NOTAMS posted"

    renderSolution (CheckPreflightBriefing c) =
        [SFText "Check preflight briefing,", SFContext c]

-- ObtainFlightAuthorization
newtype ObtainFlightAuthorization c = ObtainFlightAuthorization (Node c Context)
instance (Typeable c, RenderContext c) => SolutionType (ObtainFlightAuthorization c) where
    type SolutionEvidence (ObtainFlightAuthorization c) = AuthorizationRequest c

    validEvidence AuthorizationRequest{..} = requestApproved

    renderEvidence AuthorizationRequest{..} = " " <> requestType <> " flight authorization requested " <> timeToText requestTime <> " for location " 
                                              <> requestLocation <> if requestApproved then " approved" else " rejected"

    renderSolution (ObtainFlightAuthorization c) =
        [SFText "Obtain flight authorization for a flight according to ", SFContext c]

-- CheckAeronaticalChart
newtype CheckAeronaticalChart c = CheckAeronaticalChart (Node c Context)
instance (Typeable c, RenderContext c) => SolutionType (CheckAeronaticalChart c) where
    type SolutionEvidence (CheckAeronaticalChart c) = AeronaticalChart c

    validEvidence AeronaticalChart{..} = chartAirspaceUncontrolled

    renderEvidence AeronaticalChart{..} = " Airspace is " <> if chartAirspaceUncontrolled then " uncontrolled" else " controlled"

    renderSolution (CheckAeronaticalChart c) =
        [SFText "Check aeronatical chart,", SFContext c]

-- CheckUserManual
newtype CheckUserManual c = CheckUserManual (Node c Context)
instance (Typeable c, RenderContext c) => SolutionType (CheckUserManual c) where
    type SolutionEvidence (CheckUserManual c) = UserManual c

    -- TODO: how to do something like: if uav.limit < forecast.wind then True else False ?
    validEvidence UserManual{..} = True

    renderEvidence UserManual{..} = " User manual version " <> userManualVersion

    renderSolution (CheckUserManual c) =
        [SFText "Check user manual,", SFContext c]

-- CheckWeatherForecast
newtype CheckWeatherForecast c = CheckWeatherForecast (Node c Context)
instance (Typeable c, RenderContext c) => SolutionType (CheckWeatherForecast c) where
    type SolutionEvidence (CheckWeatherForecast c) = WeatherForecastEvidence c

    -- TODO: how to do something like: if uav.limit < forecast.wind then True else False ?
    validEvidence WeatherForecastEvidence{..} = True

    renderEvidence WeatherForecastEvidence{..} = " Weather forecast from " <> timeToText weatherForecastTime

    renderSolution (CheckWeatherForecast c) =
        [SFText "Check weather forecast,", SFContext c]

-- VisualObservation
newtype VisualObservation c = VisualObservation (Node c Context)
instance (Typeable c, RenderContext c) => SolutionType (VisualObservation c) where
    type SolutionEvidence (VisualObservation c) = VisualObservationEvidence c

    -- TODO: I am describing a process, how can I decide if it is True?
    validEvidence VisualObservationEvidence{..} = True

    renderEvidence VisualObservationEvidence{..} = " Visual observers are " <> visualObservers

    renderSolution (VisualObservation c) =
        [SFText "Visual observation of the flight area defined in", SFContext c]