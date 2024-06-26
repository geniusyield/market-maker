module GeniusYield.MarketMaker.MakerBotConfig where

import           Control.Exception                  (throwIO)
import           Data.Aeson                         (eitherDecodeFileStrict,
                                                     eitherDecodeStrict)
import           Data.Bifunctor                     (first)
import           Data.String                        (IsString (..))
import qualified Data.Yaml                          as Yaml
import           Deriving.Aeson
import           GeniusYield.MarketMaker.MakerBot   (MakerBot (..))
import           GeniusYield.MarketMaker.Prices
import           GeniusYield.MarketMaker.Strategies
import           GeniusYield.MarketMaker.User
import           System.Envy                        (FromEnv (fromEnv),
                                                     decodeEnv, env)
import           System.FilePath                    (takeExtension)

data MakerBotConfig = MakerBotConfig
  { mbcUser           ∷ !UserRaw,
    mbcDelay          ∷ !Int,
    mbcToken          ∷ !MMToken,
    mbcStrategyConfig ∷ !StrategyConfig,
    mbcPriceConfig    ∷ !PriceConfig
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] MakerBotConfig

instance FromEnv MakerBotConfig where
  fromEnv _ =
    MakerBotConfig
      <$> (forceFromJson <$> env "MBC_USER")
      <*> env "MBC_DELAY"
      <*> (forceFromJson <$> env "MBC_TOKEN")
      <*> (forceFromJson <$> env "MBC_STRATEGY_CONFIG")
      <*> (forceFromJson <$> env "MBC_PRICE_CONFIG")
   where
    forceFromJson ∷ FromJSON a ⇒ String → a
    forceFromJson = either error id . eitherDecodeStrict . fromString

eitherDecodeFileStrictJsonOrYaml ∷ FromJSON a => FilePath → IO (Either String a)
eitherDecodeFileStrictJsonOrYaml fp =
  case takeExtension fp of
    ".json" → eitherDecodeFileStrict fp
    ".yaml" → first show <$> Yaml.decodeFileEither fp
    _       → throwIO $ userError "Only .json or .yaml extensions are supported for configuration."

readMBotConfig ∷ Maybe FilePath → IO MakerBotConfig
readMBotConfig mfp = do
  e ← maybe decodeEnv eitherDecodeFileStrictJsonOrYaml mfp
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
        { mbUser  = user,
          mbDelay = mbcDelay,
          mbToken = mbcToken
        }
