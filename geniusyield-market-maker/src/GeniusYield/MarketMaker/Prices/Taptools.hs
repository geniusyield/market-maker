module GeniusYield.MarketMaker.Prices.Taptools where

import           Data.Aeson
import           Data.Aeson.Types                 (typeMismatch)
import qualified Data.Map                         as Map
import           Data.Proxy
import qualified Data.Text                        as Text
import           Data.Text.Encoding               (encodeUtf8)
import           Data.Time.Clock.POSIX            (POSIXTime)
import           GeniusYield.GYConfig             (Confidential (..))
import           GeniusYield.Imports              (Generic)
import           GeniusYield.MarketMaker.Utils
import           Network.HTTP.Client              (newManager, Manager, ManagerSettings(..), Request(..))
import           Network.HTTP.Client.TLS          (tlsManagerSettings)
import           Servant.API
import           Servant.Client


-- | Taptools time resolution for OHLC Candles
data TtResolution = TtRes3m | TtRes5m | TtRes15m | TtRes30m | TtRes1h | TtRes2h | TtRes4h | TtRes12h
                  | TtRes1d | TtRes3d | TtRes1w  | TtRes1mo
                  deriving stock (Eq, Ord, Show, Generic)

instance FromJSON TtResolution where
    parseJSON (String v) =
      let kvm = Map.fromList [ ("3m", TtRes3m), ("5m", TtRes5m), ("15m", TtRes15m), ("30m", TtRes30m), ("1h", TtRes1h), ("2h", TtRes2h),
                               ("4h", TtRes4h), ("12h", TtRes12h), ("1d", TtRes1d), ("3d", TtRes3d), ("1w", TtRes1w), ("1mo", TtRes1mo) ]
      in case Map.lookup v kvm of
        Nothing    -> typeMismatch "TtResolution" ""
        Just ttres -> pure ttres
    parseJSON invalid    = typeMismatch "TtResolution" invalid

-- instance Show TtResolution where
--   show = let kvm = Map.fromList [ (TtRes3m, "3m"), (TtRes5m, "5m"), (TtRes15m, "15m"), (TtRes30m, "30m")
--                                 , (TtRes1h, "1h"), (TtRes2h, "2h"), (TtRes4h, "4h"), (TtRes12h, "12h")
--                                 , (TtRes1d, "1d"), (TtRes3d, "3d"), (TtRes1w, "1w"), (TtRes1mo, "1M") ]
--          in  fromJust . flip Map.lookup kvm

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
  "token" :> "ohlcv" :> QueryParam "unit" TtUnit
                     :> QueryParam "interval" TtResolution
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

getTtOHLCV :: Maybe TtUnit -> Maybe TtResolution -> Maybe Int -> ClientM [TtOHLCV]
getTtOHLCV = client api

taptoolsManager :: Confidential Text.Text -> IO Manager
taptoolsManager apiKey = newManager $ tlsManagerSettings { managerModifyRequest = withHeaders }
  where
    Confidential apiKey' = apiKey

    withHeaders :: Request -> IO Request
    withHeaders req = do
      return $ req { requestHeaders = ("x-api-key", encodeUtf8 apiKey') :
                                      filter (("x-api-key" /=) . fst) (requestHeaders req)
                   }

taptoolsBaseUrl :: BaseUrl
taptoolsBaseUrl = BaseUrl Http "openapi.taptools.io" 80 "api/v1"

priceFromTaptools :: Maybe TtUnit -> Maybe TtResolution -> Maybe Int -> ClientEnv -> IO (Either ClientError [TtOHLCV])
priceFromTaptools mbUnit mbInterval mbNumIntervals = runClientM (getTtOHLCV mbUnit mbInterval mbNumIntervals)
