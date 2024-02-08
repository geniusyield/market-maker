module GeniusYield.MarketMaker.MakerBotConfig where

import           Control.Exception                  (throwIO)
import           Data.Aeson                         (eitherDecodeFileStrict,
                                                     eitherDecodeStrict)
import           Data.Bifunctor                     (first)
import           Data.String                        (IsString (..))
import qualified Data.Yaml                          as Yaml
import           Deriving.Aeson
import           GeniusYield.Api.Dex.PartialOrder   (PORefs (..))
import           GeniusYield.MarketMaker.MakerBot   (MakerBot (..))
import           GeniusYield.MarketMaker.Orphans    ()
import           GeniusYield.MarketMaker.Prices
import           GeniusYield.MarketMaker.Strategies
import           GeniusYield.MarketMaker.User
import           GeniusYield.MarketMaker.Utils
import           GeniusYield.Types
import           PlutusLedgerApi.V1                 (Address)
import           PlutusLedgerApi.V1.Scripts         (ScriptHash)
import           PlutusLedgerApi.V1.Value           (AssetClass)
import           Ply                                (ScriptRole (..),
                                                     TypedScript,
                                                     readTypedScript)
import           System.Envy                        (FromEnv (fromEnv),
                                                     decodeEnv, env)
import           System.FilePath                    (takeExtension)

data MakerBotConfig = MakerBotConfig
  { mbcUser             ∷ !UserRaw,
    mbcFPNftPolicy      ∷ !FilePath,
    mbcFPOrderValidator ∷ !FilePath,
    mbcPOConfigAddr     ∷ !GYAddressBech32,
    mbcPORefs           ∷ !PORefs,
    mbcDelay            ∷ !Int,
    mbcToken            ∷ !MMToken,
    mbcStrategyConfig   ∷ !StrategyConfig,
    mbcPriceConfig      ∷ !PriceConfig
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] MakerBotConfig

instance FromEnv MakerBotConfig where
  fromEnv _ =
    MakerBotConfig
      <$> (forceFromJson <$> env "MBC_USER")
      <*> env "MBC_FP_NFT_POLICY"
      <*> env "MBC_FP_ORDER_VALIDATOR"
      <*> (forceFromJson <$> env "MBC_PO_CONFIG_ADDR")
      <*> (forceFromJson <$> env "MBC_PO_REFS")
      <*> env "MBC_DELAY"
      <*> (forceFromJson <$> env "MBC_TOKEN")
      <*> (forceFromJson <$> env "MBC_STRATEGY_CONFIG")
      <*> (forceFromJson <$> env "MBC_PRICE_CONFIG")
   where
    forceFromJson ∷ FromJSON a ⇒ String → a
    forceFromJson = either error id . eitherDecodeStrict . fromString

eitherDecodeFileStrictJsonOrYaml :: FromJSON a => FilePath -> IO (Either String a)
eitherDecodeFileStrictJsonOrYaml fp =
  case takeExtension fp of
    ".json" -> eitherDecodeFileStrict fp
    ".yaml" -> first show <$> Yaml.decodeFileEither fp
    _       -> throwIO $ userError "Only .json or .yaml extensions are supported for configuration."

readMBotConfig ∷ Maybe FilePath → IO MakerBotConfig
readMBotConfig mfp = do
  e <- maybe decodeEnv eitherDecodeFileStrictJsonOrYaml mfp
  either (throwIO . userError) return e

buildMakerBot ∷ MakerBotConfig → IO MakerBot
buildMakerBot
  MakerBotConfig
    { mbcUser,
      mbcDelay,
      mbcToken
    } = do
    user ← getUser mbcUser
    return
      MakerBot
        { mbUser = user,
          mbDelay = mbcDelay,
          mbToken = mbcToken
        }

-- | Read the compiled scripts from the paths.
getDexInfo ∷ MakerBotConfig → IO DEXInfo
getDexInfo
  MakerBotConfig
    { mbcFPNftPolicy,
      mbcFPOrderValidator,
      mbcPOConfigAddr,
      mbcPORefs
    } = do
    dexMintRaw ← readNftPolicy
    dexValRaw ← readOrderValidator

    let porefs =
          PORefs
            { porRefNft = porRefNft mbcPORefs,
              porMintRef = porMintRef mbcPORefs,
              porValRef = porValRef mbcPORefs
            }
    return
      DEXInfo
        { dexPartialOrderValidator = dexValRaw,
          dexNftPolicy = dexMintRaw,
          dexPartialOrderConfigAddr = addressFromBech32 mbcPOConfigAddr,
          dexPORefs = porefs
        }
   where
    readNftPolicy
      ∷ IO (TypedScript 'MintingPolicyRole '[ScriptHash, Address, AssetClass])
    readNftPolicy = readTypedScript mbcFPNftPolicy

    readOrderValidator
      ∷ IO (TypedScript 'ValidatorRole '[Address, AssetClass])
    readOrderValidator = readTypedScript mbcFPOrderValidator
