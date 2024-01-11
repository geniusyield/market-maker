module GeniusYield.MarketMaker.User where

import           Deriving.Aeson
import           GeniusYield.Types

data UserRaw = UserRaw
  { urSKeyPath     :: !FilePath,
    urColl         :: !(Maybe GYTxOutRef),
    urStakeAddress :: !(Maybe GYStakeAddressBech32)
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] UserRaw

getUser :: UserRaw -> IO User
getUser UserRaw {urSKeyPath, urColl, urStakeAddress} = do
  let collateral = (,False) <$> urColl
  uSKey ← readPaymentSigningKey urSKeyPath
  pure $ User {uSKey = uSKey, uColl = collateral, uStakeAddress = urStakeAddress}

data User = User
  { uSKey         :: !GYPaymentSigningKey,
    uColl         :: !(Maybe (GYTxOutRef, Bool)),
    uStakeAddress :: !(Maybe GYStakeAddressBech32)
  }
  deriving stock (Generic, Show, Eq, Ord)
