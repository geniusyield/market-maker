module Main (main) where

import           Control.Concurrent.MVar
import           Control.Exception                         (throwIO)
import           Data.Either                               (fromLeft)
import qualified Data.Text.Lazy                            as TL
import           Data.Text.Lazy.Builder                    (toLazyText)
import           GeniusYield.Api.Dex.Constants             (dexInfoDefaultPreprod)
import           GeniusYield.GYConfig
import           GeniusYield.Imports                       ((&))
import           GeniusYield.MarketMaker.Constants         (logNS)
import           GeniusYield.MarketMaker.MakerBot          (MakerBot (..))
import           GeniusYield.MarketMaker.MakerBotConfig
import           GeniusYield.MarketMaker.Prices
import           GeniusYield.MarketMaker.Strategies
import           GeniusYield.MarketMaker.Utils             (addrUser)
import           GeniusYield.OrderBot.DataSource.Providers (connectDB)
import           GeniusYield.Test.MarketMaker.MakerBot
import           GeniusYield.Test.MarketMaker.Utils
import           GeniusYield.Types                         (GYLogConfiguration (cfgLogDirector),
                                                            GYNetworkId (..),
                                                            addressToText,
                                                            gyLog',
                                                            logNamespaceFromKatip)
import           GeniusYield.Types.Logging                 (GYLogSeverity (..),
                                                            logEnvFromKatip,
                                                            logEnvToKatip)
import           GeniusYield.Types.Providers               (gyLogInfo)
import qualified Katip                                     as K
import           System.Environment                        (getEnv)
import           Test.Tasty
import           Test.Tasty.HUnit


-----------------------------------------------------------------------
----------------------------- MAIN ------------------------------------

runSequence :: IO Bool
runSequence = do
  logRef  <- newMVar []

  frameworkCfgPath <- getEnv "TEST_FRAMEWORK_CONFIG_FILE"
  mBotConfigFile   <- Just <$> getEnv "TEST_MMBOT_CONFIG_FILE"

  coreCfg <- coreConfigIO frameworkCfgPath
  mbc     <- readMBotConfig mBotConfigFile
  mb      <- buildMakerBot mbc
  di      <-
    case cfgNetworkId coreCfg of
      GYTestnetPreprod -> pure dexInfoDefaultPreprod
      _                -> throwIO $ userError "Test supported only on Preprod."

  let netId = cfgNetworkId coreCfg
  withCfgProviders coreCfg "" $ \providers' -> do
    let gyLogGiven  = gyLog' providers'
        logEnvGiven = fromLeft (error "Absurd: Unable to get log environment") $ cfgLogDirector gyLogGiven
        testScribe = K.Scribe {liPush = \i -> modifyMVar_ logRef $ \current -> pure $ LDLog (MBLog (K._itemSeverity i & logSeverityFromKatip, K._itemNamespace i & logNamespaceFromKatip, K._itemMessage i & K.unLogStr & toLazyText & TL.unpack)) : current, scribeFinalizer = pure (), scribePermitItem = \_ -> pure True}
    logEnvNew <- logEnvFromKatip <$> K.registerScribe "test" testScribe K.defaultScribeSettings (logEnvToKatip logEnvGiven)
    let gyLogNew    = gyLogGiven { cfgLogDirector = Left logEnvNew }
        providers   = providers' { gyLog' = gyLogNew }

    gyLogInfo providers logNS $
         "Genius Yield Market Maker: "
      ++ show (addressToText $ addrUser (cfgNetworkId coreCfg) $ mbUser mb)

    c  <- connectDB netId providers
    pp <- buildPP c di (mbcPriceConfig mbc)
    executeStrategy (fixedSpreadVsMarketPriceStrategy (mbcStrategyConfig mbc)) mb netId providers pp di logRef

  finalLog <- readMVar logRef
  return . examineLog . reverse $ finalLog

examineSequence :: TestTree
examineSequence = testCase "Test Prices Providers status sequence" $ do
  result <- runSequence
  assertBool "Examined log should pass criteria" result

main :: IO ()
main = defaultMain $ testGroup "All Tests" [examineSequence]

logSeverityFromKatip :: K.Severity -> GYLogSeverity
logSeverityFromKatip K.DebugS   = GYDebug
logSeverityFromKatip K.InfoS    = GYInfo
logSeverityFromKatip K.WarningS = GYWarning
logSeverityFromKatip K.ErrorS   = GYError
logSeverityFromKatip _ = error "Absurd: Unknown log severity when called logSeverityFromKatip"
