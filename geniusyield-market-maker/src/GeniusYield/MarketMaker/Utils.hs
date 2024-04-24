module GeniusYield.MarketMaker.Utils where

import qualified Cardano.Api                      as Api
import           Data.Aeson
import           Data.Aeson.Types                 (typeMismatch)
import qualified Data.Map                         as Map
import qualified Data.Text                        as Text
import           GeniusYield.Api.Dex.PartialOrder (PORefs)
import           GeniusYield.Imports              (coerce, first, throwIO, (&))
import           GeniusYield.MarketMaker.User     (Secret (getSecret),
                                                   User (..))
import           GeniusYield.Providers.Common     (SomeDeserializeError (DeserializeErrorAssetClass))
import           GeniusYield.Scripts              (HasPartialOrderConfigAddr (..),
                                                   HasPartialOrderNftScript (..),
                                                   HasPartialOrderScript (..))
import           GeniusYield.Types
import qualified Maestro.Types.V1                 as Maestro
import           Maestro.Types.V1                 (Resolution (..))
import           PlutusLedgerApi.V1.Scripts       (ScriptHash)
import           PlutusLedgerApi.V1.Value         (AssetClass)
import           PlutusLedgerApi.V2               (Address)
import           Ply                              (ScriptRole (..), TypedScript)
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
                      deriving stock (Eq, Ord, Show)

instance FromJSON CommonResolution where
    parseJSON (String v) =
      let kvm = Map.fromList [ ("5m", CRes5m), ("15m", CRes15m), ("30m", CRes30m), ("1h", CRes1h),
                               ("4h", CRes4h), ("1d", CRes1d), ("1w", CRes1w), ("1mo", CRes1mo) ]
      in case Map.lookup v kvm of
        Nothing   -> typeMismatch "CommonResolution" ""
        Just cres -> pure cres
    parseJSON invalid    = typeMismatch "CommonResolution" invalid

class PriceResolution a where
  resolutionInherited :: Map.Map CommonResolution a

instance PriceResolution Maestro.Resolution where
  resolutionInherited = Map.fromList [ (CRes5m, Res5m), (CRes15m, Res15m), (CRes30m, Res30m), (CRes1h, Res1h)
                                     , (CRes4h, Res4h), (CRes1d, Res1d), (CRes1w, Res1w), (CRes1mo, Res1mo) ]

fromCommonResolution :: PriceResolution a => CommonResolution -> IO a
fromCommonResolution = maybe (throwIO $ userError "Undefined common price resolution.") pure
                       . flip Map.lookup resolutionInherited


-------------------------------------------------------------------------------
-- Mean, Weighted Mean, and Relative Standard Deviation
-------------------------------------------------------------------------------

mean :: Fractional a => [a] -> a
mean []     = error "mean of empty sample is undefined"
mean sample = let n = fromIntegral . length $ sample
              in  sum sample / n

weightedMean :: Fractional a => [(a,a)] -> a
weightedMean []  = error "weighted mean of empty sample is undefined"
weightedMean wps = numerator / denominator
  where
    numerator   = sum $ map (\(x, y) -> x * y) wps
    denominator = sum $ map fst wps

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
