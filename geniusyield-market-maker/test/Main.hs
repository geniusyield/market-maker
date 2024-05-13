module Main (main) where

import           Control.Concurrent.MVar
import           Control.Exception                         (throwIO)
import           GeniusYield.Api.Dex.Constants             (dexInfoDefaultPreprod)
import           GeniusYield.GYConfig
import           GeniusYield.MarketMaker.Constants         (logNS)
import           GeniusYield.MarketMaker.MakerBotConfig
import           GeniusYield.MarketMaker.MakerBot          (MakerBot(..))
import           GeniusYield.MarketMaker.Prices
import           GeniusYield.MarketMaker.Strategies
import           GeniusYield.MarketMaker.Utils             (addrUser)
import           GeniusYield.Test.MarketMaker.MakerBot
import           GeniusYield.Test.MarketMaker.Utils
import           GeniusYield.OrderBot.DataSource.Providers (connectDB)
import           GeniusYield.Types                         (GYNetworkId (..), addressToText,
                                                            gyLog', GYLog(..))
import           GeniusYield.Types.Providers               (gyLogInfo)
import           Test.Tasty
import           Test.Tasty.HUnit


-----------------------------------------------------------------------
----------------------------- MAIN ------------------------------------

runSequence :: IO Bool
runSequence = do
  logRef  <- newMVar []

  let frameworkCfgPath = "../secrets/my-atlas-config-maestro.json"
      mBotConfigFile   = Just "../secrets/my-preprod-maker-bot-config-gens-v2-test.yaml"

  coreCfg <- coreConfigIO frameworkCfgPath
  mbc     <- readMBotConfig mBotConfigFile
  mb      <- buildMakerBot mbc
  di      <-
    case cfgNetworkId coreCfg of
      GYTestnetPreprod -> pure dexInfoDefaultPreprod
      _ -> throwIO $ userError "Test supported only on Preprod."

  let netId = cfgNetworkId coreCfg
  withCfgProviders coreCfg "" $ \providers' -> do
    let gyLogGiven  = gyLog' providers'
        logRunGiven = logRun gyLogGiven
        logRunNew   = augmentedLogRun logRef logRunGiven
        gyLogNew    = gyLogGiven { logRun = logRunNew }
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
