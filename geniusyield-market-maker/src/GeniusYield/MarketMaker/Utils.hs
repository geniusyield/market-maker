module GeniusYield.MarketMaker.Utils where

import qualified Cardano.Api                      as Api
import           Data.Aeson                       (camelTo2)
import qualified Data.Text                        as Text
import           GeniusYield.Api.Dex.PartialOrder (PORefs)
import           GeniusYield.Imports              (coerce, first, (&))
import           GeniusYield.MarketMaker.User     (Secret (getSecret),
                                                   User (..))
import           GeniusYield.Providers.Common     (SomeDeserializeError (DeserializeErrorAssetClass))
import           GeniusYield.Scripts              (HasPartialOrderConfigAddr (..),
                                                   HasPartialOrderNftScript (..),
                                                   HasPartialOrderScript (..))
import           GeniusYield.Types
import qualified Maestro.Types.V1                 as Maestro
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
