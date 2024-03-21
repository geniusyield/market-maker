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
import           GeniusYield.Imports              (Generic, coerce, first, (&))
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
-- TapTools OHLCV query
-------------------------------------------------------------------------------

-- | Time resolution for OHLC Candles
data TtResolution = TtRes3m | TtRes5m | TtRes15m | TtRes30m | TtRes1h | TtRes2h | TtRes4h | TtRes12h
                  | TtRes1d | TtRes3d | TtRes1w  | TtRes1mo
                  deriving stock (Eq, Ord, Generic)

instance FromJSON TtResolution

instance Show TtResolution where
  show = let kvm = Map.fromList [ (TtRes3m, "3m"), (TtRes5m, "5m"), (TtRes15m, "15m"), (TtRes30m, "30m")
                                , (TtRes1h, "1h"), (TtRes2h, "2h"), (TtRes4h, "4h"), (TtRes12h, "12h")
                                , (TtRes1d, "1d"), (TtRes3d, "3d"), (TtRes1w, "1w"), (TtRes1mo, "1M") ]
         in  fromJust . flip Map.lookup kvm

fromResolution :: Resolution -> TtResolution
fromResolution = let kvm = Map.fromList [ (Res1m, TtRes3m), (Res5m, TtRes5m), (Res15m, TtRes15m), (Res30m, TtRes30m)
                                        , (Res1h, TtRes1h), (Res4h, TtRes4h), (Res1d, TtRes1d), (Res1w, TtRes1w), (Res1mo, TtRes1mo) ]
                 in  fromJust . flip Map.lookup kvm

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
