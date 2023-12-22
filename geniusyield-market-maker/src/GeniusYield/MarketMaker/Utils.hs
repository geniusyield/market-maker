module GeniusYield.MarketMaker.Utils where

import qualified Data.Text as Text
import GeniusYield.Api.Dex.PartialOrder (PORefs)
import GeniusYield.Imports (coerce, first)
import GeniusYield.Providers.Common (SomeDeserializeError (DeserializeErrorAssetClass))
import GeniusYield.Scripts (HasPartialOrderConfigAddr (..), HasPartialOrderNftScript (..), HasPartialOrderScript (..))
import GeniusYield.Types
import qualified Maestro.Types.V1 as Maestro
import PlutusLedgerApi.V1.Scripts (ScriptHash)
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V2 (Address)
import Ply (ScriptRole (..), TypedScript)

pkhFromSkey ∷ GYPaymentSigningKey → GYPubKeyHash
pkhFromSkey = pubKeyHash . paymentVerificationKey

addrFromSkey ∷ GYNetworkId → GYPaymentSigningKey → GYAddress
addrFromSkey netId = addressFromPubKeyHash netId . pkhFromSkey

-- | Convert Maestro's asset class to our GY type.
assetClassFromMaestro ∷ (Maestro.TokenName, Maestro.PolicyId) → Either SomeDeserializeError GYAssetClass
assetClassFromMaestro ("", "") = pure GYLovelace
assetClassFromMaestro (tokenName, policyId) = first (DeserializeErrorAssetClass . Text.pack) $ parseAssetClassWithSep '#' (coerce policyId <> "#" <> coerce tokenName)

-- | Type that encapsulates the scripts needed for the dex api.
data DEXInfo = DEXInfo
  { dexPartialOrderValidator ∷ !(TypedScript 'ValidatorRole '[Address, AssetClass]),
    dexNftPolicy ∷ !(TypedScript 'MintingPolicyRole '[ScriptHash, Address, AssetClass]),
    dexPartialOrderConfigAddr ∷ !GYAddress,
    dexPORefs ∷ !PORefs
  }

instance HasPartialOrderScript DEXInfo where
  getPartialOrderValidator = dexPartialOrderValidator

instance HasPartialOrderNftScript DEXInfo where
  getPartialOrderNftPolicy = dexNftPolicy

instance HasPartialOrderConfigAddr DEXInfo where
  getPartialOrderConfigAddr = dexPartialOrderConfigAddr
