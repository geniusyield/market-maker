module GeniusYield.MarketMaker.Prices.Taptools where

import           Data.Aeson
import           Deriving.Aeson
import           Data.Proxy
import           Data.Text                        (Text)
import           Data.Time.Clock.POSIX            (POSIXTime)
import           GeniusYield.GYConfig             (Confidential (..))
import           Network.HTTP.Client              (newManager, Manager)
import           Network.HTTP.Client.TLS          (tlsManagerSettings)
import           Servant.API
import           Servant.Client


-- | Taptools time resolution for OHLC Candles
data TtResolution =
    TtRes3m | TtRes5m | TtRes15m | TtRes30m | TtRes1h | TtRes2h | TtRes4h | TtRes12h
  | TtRes1d | TtRes3d | TtRes1w  | TtRes1M
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "TtRes"]] TtResolution

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
    TtRes1M  -> "1M"

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
