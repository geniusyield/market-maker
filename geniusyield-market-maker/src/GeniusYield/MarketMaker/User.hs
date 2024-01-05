module GeniusYield.MarketMaker.User where

import Data.Aeson (withText)
import Deriving.Aeson
import GeniusYield.Imports
import GeniusYield.Types

-- TODO: Move to Atlas.
newtype GYStakeAddressBech32 = GYStakeAddressBech32 GYStakeAddress
  deriving newtype (Show, Eq, Ord)

stakeAddressToBech32 ∷ GYStakeAddress → GYStakeAddressBech32
stakeAddressToBech32 = coerce

stakeAddressFromBech32 ∷ GYStakeAddressBech32 → GYStakeAddress
stakeAddressFromBech32 = coerce

instance ToJSON GYStakeAddressBech32 where
  toJSON (GYStakeAddressBech32 addr) = toJSON $ stakeAddressToText addr

instance FromJSON GYStakeAddressBech32 where
  parseJSON = withText "GYStakeAddressBech32" $ \t →
    case stakeAddressFromTextMaybe t of
      Just addr → pure $ GYStakeAddressBech32 addr
      Nothing → fail "cannot deserialise stake address"

data UserRaw = UserRaw
  { urSKeyPath ∷ !FilePath,
    urColl ∷ !(Maybe GYTxOutRef),
    urStakeAddress ∷ !(Maybe GYStakeAddressBech32)
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] UserRaw

getUser ∷ UserRaw → IO User
getUser UserRaw {urSKeyPath, urColl, urStakeAddress} = do
  let collateral = (,False) <$> urColl
  uSKey ← readPaymentSigningKey urSKeyPath
  pure $ User {uSKey = uSKey, uColl = collateral, uStakeAddress = urStakeAddress}

data User = User
  { uSKey ∷ !GYPaymentSigningKey,
    uColl ∷ !(Maybe (GYTxOutRef, Bool)),
    uStakeAddress ∷ !(Maybe GYStakeAddressBech32)
  }
  deriving stock (Generic, Show, Eq, Ord)
