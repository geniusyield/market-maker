module GeniusYield.MarketMaker.Utils where

import qualified Cardano.Api                      as Api
import           Control.Exception                (Exception(displayException), IOException,
                                                   evaluate, throwIO) 
import           Data.Aeson
import           Data.Aeson.Types                 (typeMismatch)
import           Data.List.NonEmpty               (NonEmpty(..))
import qualified Data.List.NonEmpty               as NE (toList)
import qualified Data.Map                         as Map
import qualified Data.Text                        as Text
import           GeniusYield.Imports              (coerce, first, (&))
import           GeniusYield.MarketMaker.User     (Secret (getSecret), User (..))
import           GeniusYield.Providers.Common     (SomeDeserializeError (DeserializeErrorAssetClass))
import           GeniusYield.Types
import qualified Maestro.Types.V1                 as Maestro
import           Maestro.Types.V1                 (Resolution (..))
import           Unsafe.Coerce                    (unsafeCoerce)
import           System.IO                        (withFile, IOMode(ReadMode), hGetContents)


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


-------------------------------------------------------------------------------
-- Common Price Resolution
-------------------------------------------------------------------------------

-- | Generic time resolution for OHLC Candles
data CommonResolution = CRes5m | CRes15m | CRes30m | CRes1h | CRes4h | CRes1d | CRes1w  | CRes1mo
  deriving stock (Eq, Ord, Show)

commonResolutionFromJSON :: Map.Map Text.Text CommonResolution
commonResolutionFromJSON = Map.fromList
  [ ("5m", CRes5m), ("15m", CRes15m), ("30m", CRes30m), ("1h", CRes1h),
    ("4h", CRes4h), ("1d", CRes1d), ("1w", CRes1w), ("1mo", CRes1mo) ]

instance FromJSON CommonResolution where
    parseJSON (String v) = case Map.lookup v commonResolutionFromJSON of
        Nothing   -> fail $ "Value " ++ show v ++ " does not correspond to a valid 'pcc_common_resolution'."
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

mean :: Fractional a => NonEmpty a -> a
mean sample = let n = fromIntegral . length $ sample
              in  sum (NE.toList sample) / n

weightedMean :: Fractional a => NonEmpty (a,a) -> a
weightedMean wxs = numerator / denominator
  where
    sum'        = sum . NE.toList
    numerator   = sum' $ (\(w, x) -> w * x) <$> wxs
    denominator = sum' $ fst <$> wxs

relStdDev :: NonEmpty Double -> Double
relStdDev sample = case sample of
  _  :| []   -> 0
  x1 :| [x2] -> abs (x1 - x2) / (x1 + x2)
  xs         -> relStdDev' xs

relStdDev' :: NonEmpty Double -> Double
relStdDev' sample = let avg = mean sample
                        sqs = (\x -> (x - avg) ** 2) <$> sample
                    in  (sqrt . mean $ sqs) / avg


-------------------------------------------------------------------------------
-- Testing helper functions
-------------------------------------------------------------------------------

readOffset :: FilePath -> IO Double
readOffset filePath = withFile filePath ReadMode $ \handle -> do
  contents <- hGetContents handle
  _ <- evaluate (length contents)  -- Forces actual read
  let parsed = reads contents :: [(Double, String)]
  case parsed of
      [(num, _)] -> return num
      _          -> return 0

handleOffsetReadError :: IOException -> IO Double
handleOffsetReadError e = throwIO $ userError $ displayException e

readBool :: FilePath -> IO Bool
readBool filePath = withFile filePath ReadMode $ \handle -> do
  contents <- hGetContents handle
  _ <- evaluate (length contents)  -- Forces actual read
  let parsed = reads contents :: [(Bool, String)]
  case parsed of
    [(bool, _)] -> return bool
    _           -> return True

handleBoolReadError :: IOException -> IO Bool
handleBoolReadError e = throwIO $ userError $ displayException e
