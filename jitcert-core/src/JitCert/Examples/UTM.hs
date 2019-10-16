{-|

Description: A small example of using the assurance case DSL

This module makes use of the JITCert assurance case DSL in order to create some
structured arguments around flying unmanned aircrafts under FAA Part 107.  Moreso
than a proper assurance case, it is intended as a proof of concept to test
features of the DSL. 

It is called UTM because the UTM standard will eventually be complementing or
replacing the Part 107. "UTM" also rolls better than "FAA Part 107"

-}

{-# LANGUAGE RankNTypes #-}
module JitCert.Examples.UTM where

import           JitCert.Context
import           JitCert.DotBackend
import           JitCert.UTM.Combinators
import           JitCert.UTM.Context
import           JitCert.UTM.Solution
import           JitCert.GSN.Builder
import           JitCert.GSN.Builder.Internal   ( runBuilderT )
import           JitCert.GSN.Types

-- The UTM based example
runUTM :: IO ()
runUTM = do
  (g, _) <- runBuilderT utm
  render "utm.dot" g

utm :: Monad m => BuilderT m ()
utm = do
  -- Top Goal --
  -- The main goal is to conduct a safe flight, per guidance from Part 107
  gFlight     <- goalAnd [SFText "Flight can be safely conducted"]

  cPart107    <- context @Part107 "p" Nothing
  cFlightPlan <- context @FlightPlan "f" Nothing
  doc         <- context @Documentation "d" Nothing
  cWeatherForecast <- context @WeatherForecast "w" Nothing
  addEdge gFlight cPart107
  addEdge gFlight doc
  addEdge gFlight cFlightPlan

  -- Top strategy --
  -- To achieve the main goal of a safe flight, we want to mitigate all relevant flight hazards
  sAllHazardsAreSufficientlyMitigated <- strategyAnd
    [SFText "All flight hazards are sufficiently mitigated"]
  addEdge gFlight sAllHazardsAreSufficientlyMitigated

  -- Airborne hazards --
  -- We arbitrarily divide hazards to airborne, ground and other 
  gAllAirborneHazardsAreSufficientlyMitigated <- goalAnd
    [SFText "All Airborne Hazards Are Sufficiently Mitigated"]
  addEdge sAllHazardsAreSufficientlyMitigated
          gAllAirborneHazardsAreSufficientlyMitigated

  -- Ground hazards --
  -- Encompass all possible ground hazards, such as flying over populated areas, having enough crew to properly
  -- ensure safety, cordon off the fligh area etc. We expect the operator to address all relevant ground hazards
  -- in Safety Risk Assesment
  gAllGroundHazardsAreSufficientlyMitigated <- goalAnd
    [SFText "All Ground Hazards Are Sufficiently Mitigated"]
  addEdge sAllHazardsAreSufficientlyMitigated
          gAllGroundHazardsAreSufficientlyMitigated
  snSafetyRiskAssesment <- solution
    (SafetyRiskAssesment cFlightPlan $ AccordingTo cPart107)
  addEdge gAllGroundHazardsAreSufficientlyMitigated snSafetyRiskAssesment

  -- Other hazards
  -- somewhat arbitrarily the other hazards are related to the readiness of the pilot him/herself
  -- and the airplane
  gOtherHazardsAreSufficientlyMitigated <- goalAnd
    [SFText "Other Hazards Are Sufficiently Mitigated"]
  addEdge sAllHazardsAreSufficientlyMitigated
          gOtherHazardsAreSufficientlyMitigated

  -- Weather is OK --
  gWeatherIsOK <- templateWeatherIsOK doc cWeatherForecast
  addEdge gAllAirborneHazardsAreSufficientlyMitigated gWeatherIsOK

  -- Airspace is OK -- 
  gAirspaceIsOK <- goalAnd [SFText "Airspace is OK"]
  addEdge gAllAirborneHazardsAreSufficientlyMitigated gAirspaceIsOK

  -- Check for incoming aircrafts
  gNoIncomingAircraft <- goalAnd
    [SFText "No imcoming aircrafts in the flight area"]
  addEdge gAllAirborneHazardsAreSufficientlyMitigated gNoIncomingAircraft
  snVisualObservation <- solution (VisualObservation cFlightPlan)
  addEdge gNoIncomingAircraft snVisualObservation

  -- Controlled airspace -- 
  sControlledAirspace <- strategyOr
    [SFText "Strategy for a flight that occurs in a controlled airspace"]
  addEdge gAirspaceIsOK sControlledAirspace

  -- LAANC enabled controlled airspace
  gLAANCEnabledControlledAirspace <- goalOr
    [SFText "Obtain LAANC authorization"]
  addEdge sControlledAirspace gLAANCEnabledControlledAirspace
  snLANNCAutorization <- solution (ObtainFlightAuthorization cFlightPlan)
  addEdge gLAANCEnabledControlledAirspace snLANNCAutorization

  -- UTM enabled controlled airspace --
  gUTMEnabledControlledAirspace <- goalOr [SFText "Obtain UTM authorization"]
  addEdge sControlledAirspace gUTMEnabledControlledAirspace
  snUTMAutorization <- solution (ObtainFlightAuthorization cFlightPlan)
  addEdge gUTMEnabledControlledAirspace snUTMAutorization

  -- Other controlled airspace (waiwer) --
  gOtherControlledAirspace <- goalOr
    [SFText "Obtain Certificate-Of-Authorization (COA)"]
  addEdge sControlledAirspace gOtherControlledAirspace
  snCOA <- solution (ObtainFlightAuthorization cFlightPlan)
  addEdge gOtherControlledAirspace snCOA

  -- Uncontrolled airspace
  sUncontrolledAirspace <- templateUncontrolledAirspaceFlight cFlightPlan
  addEdge gAirspaceIsOK sUncontrolledAirspace

  -- Pilot is ready -- 
  gRemotePilotIsReadyToFly <- templatePilotIsReadyToFly doc
  addEdge gOtherHazardsAreSufficientlyMitigated gRemotePilotIsReadyToFly

  -- Airplane is ready (template)
  gAirplaneIsReadyToFly <- templateAirplaneIsReadyToFly doc
  addEdge gOtherHazardsAreSufficientlyMitigated gAirplaneIsReadyToFly





-- | A UTM argument template around whether the airplane is ready to fly
templateAirplaneIsReadyToFly
  :: Monad m
  => Ctx Documentation   -- ^ The context node for the documentation
  -> BuilderT m (Node c Goal)
templateAirplaneIsReadyToFly doc = do
  gAirplaneIsReadyToFly <- goalAnd [SFText "Airplane is ready to fly"]
  gAirplaneWeightIsOK   <- goalAnd
    [SFText "Airplane weight is withing allowed limits"]
  addEdge gAirplaneIsReadyToFly gAirplaneWeightIsOK
  snCheckAirplaneWeight <- solution (CheckAirplaneWeight doc)
  addEdge gAirplaneWeightIsOK snCheckAirplaneWeight

  gAirplaneIsRegistered <- goalAnd [SFText "Airplane is registered with FAA"]
  addEdge gAirplaneIsReadyToFly gAirplaneIsRegistered
  snAirplaneHasRegistrationNumber <- solution (ValidFaaRegistration doc)
  addEdge gAirplaneIsRegistered snAirplaneHasRegistrationNumber

  gAirplaneIsAirworthy <- goalAnd [SFText "Airplane is airworthy"]
  addEdge gAirplaneIsReadyToFly gAirplaneIsAirworthy
  snPreflightChecks <- solution (PreflightChecks doc)
  addEdge gAirplaneIsAirworthy snPreflightChecks

  return gAirplaneIsReadyToFly


-- | A UTM argument template around whether the pilot is ready to fly
templatePilotIsReadyToFly
  :: Monad m
  => Ctx Documentation   -- ^ The context node for the documentation
  -> BuilderT m (Node c Goal)
templatePilotIsReadyToFly doc = do
  gRemotePilotIsReadyToFly    <- goalAnd [SFText "Remote pilot is ready to fly"]

  gPilotHasValidCertification <- goalAnd
    [SFText "Pilot holds a valid UAS certification"]
  addEdge gRemotePilotIsReadyToFly gPilotHasValidCertification
  snUASCertification <- solution (ValidUasCertificate doc)
  addEdge gPilotHasValidCertification snUASCertification

  gPilotHasNoMedicalConditions <- goalAnd
    [SFText "Pilot has no known medical conditions and is ready to fly"]
  addEdge gRemotePilotIsReadyToFly gPilotHasNoMedicalConditions
  snMedicalConditionsChecklist <- solution (CheckMedicalCondition doc)
  addEdge gPilotHasNoMedicalConditions snMedicalConditionsChecklist

  return gRemotePilotIsReadyToFly

-- | A UTM argument template for checking whether the weather is OK for a safe flight
templateWeatherIsOK
  :: Monad m
  => Ctx Documentation   -- ^ The context node for the documentation
  -> Ctx WeatherForecast   -- ^ The context node for the weather forecast
  -> BuilderT m (Node c Goal)
templateWeatherIsOK doc cWeatherForecast = do
  gWeatherIsOK <- goalAnd [SFText "Weather is OK"]
  addEdge gWeatherIsOK cWeatherForecast

  gWindBelowUAVLimits <- goalAnd [SFText "Wind speed within platform limits"]
  addEdge gWeatherIsOK gWindBelowUAVLimits
  snCheckUserManual <- solution (CheckUserManual doc)
  addEdge gWindBelowUAVLimits snCheckUserManual

  gVisibilityGt3SM <- goalAnd
    [SFText "Visibility is greater than 3 statutory miles"]
  addEdge gWeatherIsOK gVisibilityGt3SM
  snCheckForecastedVisbility <- solution (CheckWeatherForecast cWeatherForecast)
  addEdge gVisibilityGt3SM snCheckForecastedVisbility

  gCloudClearance <- goalAnd
    [SFText "Cloud clearance is at least 500ft vertical and 2000ft horizontal"]
  addEdge gWeatherIsOK gCloudClearance
  snCheckForecastedCloudCeiling <- solution
    (CheckWeatherForecast cWeatherForecast)
  addEdge gCloudClearance snCheckForecastedCloudCeiling

  return gWeatherIsOK

-- | A UTM argument template for a strategy in uncontrolled airspace
-- can be safely conducted
templateUncontrolledAirspaceFlight
  :: Monad m
  => Ctx FlightPlan   -- ^ The context node for the flight plan
  -> BuilderT m (Node c Strategy)
templateUncontrolledAirspaceFlight cFlightPlan = do
  sUncontrolledAirspace <- strategyOr
    [SFText "Strategy for a flight that occurs in uncontrolled airspace"]
  cB4UFlyApp <- context @B4UFlyApp "b" Nothing
  addEdge sUncontrolledAirspace cB4UFlyApp
  -- Check charts to make sure the flight is indeed in an uncontrolled airspace
  gUncontrolledAirspace <- goalAnd
    [SFText "The flight is conducted in uncontrolled airspace"]
  addEdge sUncontrolledAirspace gUncontrolledAirspace
  snCheckAeronauticalChart <- solution (CheckAeronaticalChart cFlightPlan)
  addEdge gUncontrolledAirspace snCheckAeronauticalChart
  -- No NOTAMs
  sNoNOTAMs <- goalAnd [SFText "No NOTAMs are posted for the flight area"]
  addEdge sUncontrolledAirspace sNoNOTAMs
  snCheckPreflightBriefings <- solution (CheckPreflightBriefing cFlightPlan)
  addEdge sNoNOTAMs snCheckPreflightBriefings

  return sUncontrolledAirspace