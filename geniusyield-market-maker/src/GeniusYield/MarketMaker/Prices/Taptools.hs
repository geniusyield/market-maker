module GeniusYield.MarketMaker.Prices.Taptools where

import           Data.Aeson
import           Data.Aeson.Types                 (typeMismatch)
import qualified Data.Map                         as Map
import           Data.Proxy
import           Data.Text                        (Text)
import           Data.Time.Clock.POSIX            (POSIXTime)
import           GeniusYield.GYConfig             (Confidential (..))
import           GeniusYield.Imports              (Generic)
import           GeniusYield.MarketMaker.Utils
import           Network.HTTP.Client              (newManager, Manager)
import           Network.HTTP.Client.TLS          (tlsManagerSettings)
import           Servant.API
import           Servant.Client


-- | Taptools time resolution for OHLC Candles
data TtResolution = TtRes3m | TtRes5m | TtRes15m | TtRes30m | TtRes1h | TtRes2h | TtRes4h | TtRes12h
                  | TtRes1d | TtRes3d | TtRes1w  | TtRes1mo
                  deriving stock (Eq, Ord, Show, Generic)

taptoolsResolutionFromJSON :: Map.Map Text TtResolution
taptoolsResolutionFromJSON = Map.fromList
  [ ("3m", TtRes3m), ("5m", TtRes5m), ("15m", TtRes15m), ("30m", TtRes30m), ("1h", TtRes1h), ("2h", TtRes2h),
    ("4h", TtRes4h), ("12h", TtRes12h), ("1d", TtRes1d), ("3d", TtRes3d), ("1w", TtRes1w), ("1mo", TtRes1mo) ]

instance FromJSON TtResolution where
    parseJSON (String v) = case Map.lookup v taptoolsResolutionFromJSON of
        Nothing    -> fail $ "Value " ++ show v ++ " does not correspond to a valid 'ttc_resolution_override'."
        Just ttres -> pure ttres
    parseJSON invalid    = typeMismatch "TtResolution" invalid

instance ToHttpApiData TtResolution where
  toQueryParam ttres = case ttres of
    TtRes3m  -> "3m"
    TtRes5m  -> "5m"
    TtRes15m -> "15m"
    TtRes30m -> "30m"
    TtRes1h  -> "1h"
    TtRes2h  -> "2h"
    TtRes4h  -> "4h"
    TtRes12h -> "12h"
    TtRes1d  -> "1d"
    TtRes3d  -> "3d"
    TtRes1w  -> "1w"
    TtRes1mo -> "1M"

instance PriceResolution TtResolution where
  resolutionInherited = Map.fromList [ (CRes5m, TtRes5m), (CRes15m, TtRes15m), (CRes30m, TtRes30m), (CRes1h, TtRes1h)
                                     , (CRes4h, TtRes4h), (CRes1d, TtRes1d), (CRes1w, TtRes1w), (CRes1mo, TtRes1mo) ]

type TtUnit = String

type TtAPI =
  "token" :> "ohlcv" :> Header "x-api-key" Text
                     :> QueryParam "unit" TtUnit
                     :> QueryParam' '[Required, Strict] "interval" TtResolution
                     :> QueryParam "numIntervals" Int
                     :> Get '[JSON] [TtOHLCV]

data TtOHLCV = TtOHLCV
  { close  :: !Double
  , high   :: !Double
  , low    :: !Double
  , open   :: !Double
  , time   :: !POSIXTime
  , volume :: !Double
  } deriving stock Show

instance FromJSON TtOHLCV where
  parseJSON = withObject "TtOHLCV" $ \v ->
    TtOHLCV <$> v .: "close"
            <*> v .: "high"
            <*> v .: "low"
            <*> v .: "open"
            <*> v .: "time"
            <*> v .: "volume"

api :: Proxy TtAPI
api = Proxy

getTtOHLCV :: Maybe Text -> Maybe TtUnit -> TtResolution -> Maybe Int -> ClientM [TtOHLCV]
getTtOHLCV = client api

taptoolsManager :: IO Manager
taptoolsManager = newManager tlsManagerSettings

taptoolsBaseUrl :: BaseUrl
taptoolsBaseUrl = BaseUrl Http "openapi.taptools.io" 80 "api/v1"

priceFromTaptools :: Confidential Text -> TtUnit -> TtResolution -> Int -> ClientEnv -> IO (Either ClientError [TtOHLCV])
priceFromTaptools apiKey unit resolution numIntervals = runClientM (getTtOHLCV (Just apiKey') (Just unit) resolution  (Just numIntervals))
  where
    Confidential apiKey' = apiKey
