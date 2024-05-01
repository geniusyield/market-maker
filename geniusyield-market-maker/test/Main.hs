module Main (main) where

import           Control.Concurrent.MVar
import           Control.Exception                         (throwIO)
import           GeniusYield.GYConfig
import           GeniusYield.MarketMaker.MakerBot
import           GeniusYield.MarketMaker.MakerBotConfig
import           GeniusYield.MarketMaker.Prices
import           GeniusYield.MarketMaker.Strategies
import           GeniusYield.MarketMaker.Utils             (addrUser)
import           GeniusYield.MarketMaker.TestUtils
import           GeniusYield.OrderBot.DataSource.Providers (connectDB)
import           GeniusYield.Types                         (addressToText, gyLog', GYLog(..))
-- import           System.Environment
import           Test.Tasty
import           Test.Tasty.HUnit


-----------------------------------------------------------------------
----------------------------- MAIN ------------------------------------

{-
parseArgs :: IO (String, FilePath, Maybe FilePath)
parseArgs = do
  args <- getArgs
  case args of
    [action, frameworkCfgPath, mBotConfigFile] -> return (action, frameworkCfgPath, Just mBotConfigFile)
    [action, frameworkCfgPath]                 -> return (action, frameworkCfgPath, Nothing)
    _ ->
      throwIO
        . userError
        $ unlines
          [ "Expected 2 or 3 command line arguments, in order: ",
            "  1. Action to execute, either 'Run' or 'Cancel'.",
            "  2. Path to the Atlas config-file.",
            "  3. Path to the maker bot config file (optional). If not provided, required information is fetched from environment variables."
          ]
-}

runSequence :: IO Bool
runSequence = do
  logRef  <- newMVar []

  -- (action, frameworkCfgPath, mBotConfigFile) <- parseArgs
  let action           = "Run"
  let frameworkCfgPath = "../secrets/my-atlas-config-maestro.json"
      mBotConfigFile   = Just "../secrets/my-preprod-maker-bot-config-gens-v2.yaml"

  coreCfg <- coreConfigIO frameworkCfgPath
  mbc     <- readMBotConfig mBotConfigFile
  mb      <- buildMakerBot mbc
  di      <- getDexInfo mbc

  putStrLn $ "Genius Yield Market Maker: "
    ++ show (addressToText $ addrUser (cfgNetworkId coreCfg) $ mbUser mb)

  let netId = cfgNetworkId coreCfg
  withCfgProviders coreCfg "" $ \providers' -> do
    let gyLogGiven  = gyLog' providers'
        logRunGiven = logRun gyLogGiven
        logRunNew   = augmentedLogRun logRef logRunGiven
        gyLogNew    = gyLogGiven { logRun = logRunNew }
        providers   = providers' { gyLog' = gyLogNew }

    case action of
      "Run"    -> do
        c  <- connectDB netId providers
        pp <- buildPP c di (mbcPriceConfig mbc)
        executeStrategy (fixedSpreadVsMarketPriceStrategy (mbcStrategyConfig mbc)) mb netId providers pp di logRef
      "Cancel" -> cancelAllOrders' mb netId providers di
      _        -> throwIO . userError $ "Action '" ++ action ++ "' not supported. Check cli arguments."

  finalLog <- readMVar logRef
  return $ examineLog finalLog

examineSequence :: TestTree
examineSequence = testCase "Test Prices Providers status sequence" $ do
  result <- runSequence
  assertBool "Examined log should pass criteria" result

main :: IO ()
main = defaultMain $ testGroup "All Tests" [examineSequence]
