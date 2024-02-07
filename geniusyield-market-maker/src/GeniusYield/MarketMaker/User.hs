module GeniusYield.MarketMaker.User where

import           Control.Applicative ((<|>))
import           Control.Arrow       ((>>>))
import           Data.Maybe          (fromMaybe)
import           Data.Word           (Word32)
import           Deriving.Aeson
import           GeniusYield.Types
import           GHC.IO              (throwIO)

data MnemonicWallet = MnemonicWallet
  { mnemonic :: !Mnemonic,
  -- ^ Mnemonic (seed phrase).
    accIx    :: !(Maybe Word32),
  -- ^ Account index.
    addrIx   :: !(Maybe Word32)
  -- ^ Payment address index.
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] MnemonicWallet

-- Interface is not type clean (doesn't represent exclusion of both signing key file path and mnemonic) but is done to prevent a breaking change.
data UserRaw = UserRaw
  { urSKeyPath     :: !(Maybe FilePath),
    urColl         :: !(Maybe GYTxOutRef),
    urStakeAddress :: !(Maybe GYStakeAddressBech32),
    urMnemonic     :: !(Maybe MnemonicWallet)
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] UserRaw

newtype Secret a = Secret {getSecret :: a}
    deriving stock (Eq, Ord)

instance Show a => Show (Secret a) where
    show (Secret x) = replicate (length $ show x) '*'

getUser :: UserRaw -> IO User
getUser UserRaw {urSKeyPath, urColl, urStakeAddress, urMnemonic} = do
  let collateral = (,False) <$> urColl
      urStakeCred = stakeAddressToCredential . stakeAddressFromBech32 <$> urStakeAddress
  (uSKey, uStakeCred) <- case (urSKeyPath, urMnemonic) of
    (Nothing, Nothing) -> throwIO $ userError "Path to signing key (normal or extended) file or mnemonic seed phrase must be provided"
    (Just _, Just _) -> throwIO $ userError "Only one of signing key file or mnemonic must be given"
    (Just keyFP, Nothing) -> (, urStakeCred) <$> readSomePaymentSigningKey keyFP
    (Nothing, Just MnemonicWallet{..}) ->
      let wk' = walletKeysFromMnemonicIndexed mnemonic (fromMaybe 0 accIx) (fromMaybe 0 addrIx) in
      case wk' of
        Left e   -> throwIO $ userError e
        Right wk ->
          let skey = walletKeysToExtendedPaymentSigningKey wk
              addr = walletKeysToAddress wk GYMainnet  -- @GYMainnet@ as since we are dealing with credential, network is irrelevant.
              stakeCred = addressToStakeCredential addr
          in pure (AGYExtendedPaymentSigningKey skey, urStakeCred <|> stakeCred)  -- Giving preference to @urStakeCred@.

  pure $ User {uSKey = Secret uSKey, uColl = collateral, uStakeCred = uStakeCred}


data User = User
  { uSKey      :: !(Secret GYSomePaymentSigningKey),
    uColl      :: !(Maybe (GYTxOutRef, Bool)),
    uStakeCred :: !(Maybe GYStakeCredential)
  }
  deriving stock (Show, Eq, Ord)

uSKeyToSomeSigningKey :: User -> GYSomeSigningKey
uSKeyToSomeSigningKey = uSKey >>> getSecret >>> somePaymentSigningKeyToSomeSigningKey
