module GeniusYield.MarketMaker.Utils where

import qualified Cardano.Api                      as Api
import           Data.Aeson
import           Data.List.NonEmpty               (NonEmpty(..))
import qualified Data.List.NonEmpty               as NE (toList)
import qualified Data.Text                        as Text
import           GeniusYield.Imports              (coerce, first, (&))
import           GeniusYield.MarketMaker.User     (Secret (getSecret), User (..))
import           GeniusYield.Providers.Common     (SomeDeserializeError (DeserializeErrorAssetClass))
import           GeniusYield.Types
import qualified Maestro.Types.V1                 as Maestro
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
