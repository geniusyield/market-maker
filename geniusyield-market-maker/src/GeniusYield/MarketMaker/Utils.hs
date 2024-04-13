module GeniusYield.MarketMaker.Utils where

import qualified Cardano.Api                      as Api
import           Data.Aeson
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromJust)
import           Data.Proxy
import qualified Data.Text                        as Text
import           Data.Text.Encoding               (encodeUtf8)
import           Data.Time.Clock.POSIX            (POSIXTime)
import           GeniusYield.Api.Dex.PartialOrder (PORefs)
import           GeniusYield.GYConfig             (Confidential (..))
import           GeniusYield.Imports              (Generic, coerce, first, throwIO, (&))
import           GeniusYield.MarketMaker.User     (Secret (getSecret),
                                                   User (..))
import           GeniusYield.Providers.Common     (SomeDeserializeError (DeserializeErrorAssetClass))
import           GeniusYield.Scripts              (HasPartialOrderConfigAddr (..),
                                                   HasPartialOrderNftScript (..),
                                                   HasPartialOrderScript (..))
import           GeniusYield.Types
import qualified Maestro.Types.V1                 as Maestro
import           Maestro.Types.V1                 (Resolution (..))
import           Network.HTTP.Client              (newManager, Manager, ManagerSettings(..), Request(..))
import           Network.HTTP.Client.TLS          (tlsManagerSettings)
import           PlutusLedgerApi.V1.Scripts       (ScriptHash)
import           PlutusLedgerApi.V1.Value         (AssetClass)
import           PlutusLedgerApi.V2               (Address)
import           Ply                              (ScriptRole (..), TypedScript)
import           Servant.API
import           Servant.Client
import           Unsafe.Coerce                    (unsafeCoerce)

camelToSnake :: String -> String
camelToSnake = camelTo2 '_'

pkhUser :: User -> GYPaymentKeyHash
pkhUser User {uSKey} = case getSecret uSKey of
  AGYPaymentSigningKey skey -> paymentKeyHash . paymentVerificationKey $ skey
  AGYExtendedPaymentSigningKey skey -> extendedPaymentSigningKeyToApi skey & Api.getVerificationKey & Api.verificationKeyHash & unsafeCoerce & paymentKeyHashFromApi  -- Usage of `unsafeCoerce` here as Atlas's key hash types need an overhaul since it is not powerful enough to cater for all the relevant cases.

addrUser :: GYNetworkId -> User -> GYAddress
addrUser netId user = addressFromCredential netId
  (GYPaymentCredentialByKey $ pkhUser user)
  (uStakeCred user)

-- | Convert Maestro's asset class to our GY type.
assetClassFromMaestro :: (Maestro.TokenName, Maestro.PolicyId) → Either SomeDeserializeError GYAssetClass
assetClassFromMaestro ("", "") = pure GYLovelace
assetClassFromMaestro (tokenName, policyId) = first (DeserializeErrorAssetClass . Text.pack) $ parseAssetClassWithSep '#' (coerce policyId <> "#" <> coerce tokenName)

-- | Type that encapsulates the scripts needed for the dex api.
data DEXInfo = DEXInfo
  { dexPartialOrderValidator  :: !(TypedScript 'ValidatorRole '[Address, AssetClass]),
    dexNftPolicy              :: !(TypedScript 'MintingPolicyRole '[ScriptHash, Address, AssetClass]),
    dexPartialOrderConfigAddr :: !GYAddress,
    dexPORefs                 :: !PORefs
  }

instance HasPartialOrderScript DEXInfo where
  getPartialOrderValidator = dexPartialOrderValidator

instance HasPartialOrderNftScript DEXInfo where
  getPartialOrderNftPolicy = dexNftPolicy

instance HasPartialOrderConfigAddr DEXInfo where
  getPartialOrderConfigAddr = dexPartialOrderConfigAddr


-------------------------------------------------------------------------------
-- Common Price Resolution
-------------------------------------------------------------------------------

-- | Generic time resolution for OHLC Candles
data CommonResolution = CRes5m | CRes15m | CRes30m | CRes1h | CRes4h
                      | CRes1d | CRes1w  | CRes1mo
                      deriving stock (Eq, Ord, Show, Generic)

instance FromJSON CommonResolution

class PriceResolution a where
  resolutionInherited :: Map.Map CommonResolution a

instance PriceResolution Maestro.Resolution where
  resolutionInherited = Map.fromList [ (CRes5m, Res5m), (CRes15m, Res15m), (CRes30m, Res30m), (CRes1h, Res1h)
                                     , (CRes4h, Res4h), (CRes1d, Res1d), (CRes1w, Res1w), (CRes1mo, Res1mo) ]

fromCommonResolution :: PriceResolution a => CommonResolution -> IO a
fromCommonResolution = maybe (throwIO $ userError "Undefined common price resolution.") pure
                       . flip Map.lookup resolutionInherited


-------------------------------------------------------------------------------
-- TapTools OHLCV query
-------------------------------------------------------------------------------

-- | Taptools time resolution for OHLC Candles
data TtResolution = TtRes3m | TtRes5m | TtRes15m | TtRes30m | TtRes1h | TtRes2h | TtRes4h | TtRes12h
                  | TtRes1d | TtRes3d | TtRes1w  | TtRes1mo
                  deriving stock (Eq, Ord, Generic)

instance FromJSON TtResolution

instance Show TtResolution where
  show = let kvm = Map.fromList [ (TtRes3m, "3m"), (TtRes5m, "5m"), (TtRes15m, "15m"), (TtRes30m, "30m")
                                , (TtRes1h, "1h"), (TtRes2h, "2h"), (TtRes4h, "4h"), (TtRes12h, "12h")
                                , (TtRes1d, "1d"), (TtRes3d, "3d"), (TtRes1w, "1w"), (TtRes1mo, "1M") ]
         in  fromJust . flip Map.lookup kvm

instance PriceResolution TtResolution where
  resolutionInherited = Map.fromList [ (CRes5m, TtRes5m), (CRes15m, TtRes15m), (CRes30m, TtRes30m), (CRes1h, TtRes1h)
                                     , (CRes4h, TtRes4h), (CRes1d, TtRes1d), (CRes1w, TtRes1w), (CRes1mo, TtRes1mo) ]

type TtUnit = String; type TtInterval = String

type TtAPI =
  "token" :> "ohlcv" :> QueryParam "unit" TtUnit
                     :> QueryParam "interval" TtInterval
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

getTtOHLCV :: Maybe TtUnit -> Maybe TtInterval -> Maybe Int -> ClientM [TtOHLCV]
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

priceFromTaptools :: Maybe TtUnit -> Maybe TtInterval -> Maybe Int -> ClientEnv -> IO (Either ClientError [TtOHLCV])
priceFromTaptools mbUnit mbInterval mbNumIntervals = runClientM (getTtOHLCV mbUnit mbInterval mbNumIntervals)


-------------------------------------------------------------------------------
-- Relative Standard Deviation
-------------------------------------------------------------------------------

mean :: Fractional a => [a] -> a
mean []     = error "mean of empty sample is undefined"
mean sample = let n = fromIntegral . length $ sample
              in  sum sample / n

relStdDev :: [Double] -> Double
relStdDev sample = case sample of
  []       -> error "std dev of empty sample is undefined"
  [_]      -> 0
  [x1, x2] -> abs (x1 - x2) / (x1 + x2)
  xs       -> relStdDev' xs

relStdDev' :: [Double] -> Double
relStdDev' sample = let avg = mean sample
                        sqs = (\x -> (x - avg) ** 2) <$> sample
                    in  (sqrt . mean $ sqs) / avg
