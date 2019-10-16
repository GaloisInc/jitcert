module JitCert.UTM.Context where

import           JitCert.GSN.Types
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Text.Lazy                as T
import           Data.Time.Clock                ( UTCTime )

-- UTM --
data WeatherForecast = WeatherForecast {
    weatherForecastLocation :: T.Text
  , weatherForecastTime :: T.Text
  }

instance RenderContext WeatherForecast where
    renderContextTypeReference Proxy = "weather forecast"
    renderContext (WeatherForecast location _) = location

newtype B4UFlyApp = B4UFlyApp {
    appVersion :: T.Text
  }

instance RenderContext B4UFlyApp where
    renderContextTypeReference Proxy = "B4UFlyApp"
    renderContext (B4UFlyApp version) = version

newtype Part107 = Part107 {
    part107Version :: T.Text
  }

instance RenderContext Part107 where
    renderContextTypeReference Proxy = "Part 107"
    renderContext (Part107 version) = version

data FlightPlan = FlightPlan {
    flightArea :: T.Text
  , flightTime :: UTCTime
  , flightAglFt :: Int
  }

instance RenderContext FlightPlan where
    renderContextTypeReference Proxy = "FlightPlan"
    renderContext (FlightPlan area _ _) = area