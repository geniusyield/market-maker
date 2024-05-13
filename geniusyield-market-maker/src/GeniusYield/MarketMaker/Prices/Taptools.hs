module GeniusYield.MarketMaker.Prices.Taptools where

import           Data.Aeson
import           Deriving.Aeson
import           Data.Proxy
import           Data.Text                        (Text)
import           Data.Text.Encoding               (encodeUtf8)
import           Data.Time.Clock.POSIX            (POSIXTime)
import           Data.Word                        (Word8)
import           GeniusYield.GYConfig             (Confidential (..))
import           Network.HTTP.Client              (newManager, Manager, ManagerSettings(..), Request(..))
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
  "token" :> "ohlcv" :> QueryParam "unit" TtUnit
                     :> QueryParam' '[Required, Strict] "interval" TtResolution
                     :> QueryParam "numIntervals" Word8
                     :> Get '[JSON] [TtOHLCV]

data TtOHLCV = TtOHLCV
  { close  :: !Double
  , high   :: !Double
  , low    :: !Double
  , open   :: !Double
  , time   :: !POSIXTime
  , volume :: !Double
  } deriving stock (Show, Generic)

instance FromJSON TtOHLCV

api :: Proxy TtAPI
api = Proxy

getTtOHLCV :: Maybe TtUnit -> TtResolution -> Maybe Word8 -> ClientM [TtOHLCV]
getTtOHLCV = client api

taptoolsManager :: Confidential Text -> IO Manager
taptoolsManager apiKey = newManager $ tlsManagerSettings { managerModifyRequest = withHeaders }
  where
    Confidential apiKey' = apiKey

    withHeaders :: Request -> IO Request
    withHeaders req = do
      return $ req { requestHeaders = ("x-api-key", encodeUtf8 apiKey') :
                                      filter (("x-api-key" /=) . fst) (requestHeaders req)
                   }

taptoolsBaseUrl :: BaseUrl
taptoolsBaseUrl = BaseUrl Https "openapi.taptools.io" 443 "api/v1"

priceFromTaptools :: TtUnit -> TtResolution -> Word8 -> ClientEnv -> IO (Either ClientError [TtOHLCV])
priceFromTaptools unit resolution numIntervals = runClientM (getTtOHLCV (Just unit) resolution  (Just numIntervals))
