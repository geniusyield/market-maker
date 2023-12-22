module Main (main) where

import Control.Exception (throwIO)
import GeniusYield.GYConfig
import GeniusYield.MarketMaker.MakerBot
import GeniusYield.MarketMaker.MakerBotConfig
import GeniusYield.MarketMaker.Prices
import GeniusYield.MarketMaker.Strategies
import GeniusYield.OrderBot.DataSource.Providers (connectDB)
import System.Environment

-----------------------------------------------------------------------
----------------------------- MAIN ------------------------------------

parseArgs ∷ IO (String, FilePath, Maybe FilePath)
parseArgs = do
  args ← getArgs
  case args of
    [action, frameworkCfgPath, mBotConfigFile] → return (action, frameworkCfgPath, Just mBotConfigFile)
    [action, frameworkCfgPath] → return (action, frameworkCfgPath, Nothing)
    _ →
      throwIO
        . userError
        $ unlines
          [ "Expected 2 or 3 command line arguments, in order: ",
            "  1. Action to execute, either 'Run' or 'Cancel'.",
            "  2. Path to the Atlas config-file.",
            "  3. Path to the maker bot config file (optional). If not provided, required information is fetched from environment variables."
          ]

main ∷ IO ()
main = do
  (action, frameworkCfgPath, mBotConfigFile) ← parseArgs

  coreCfg ← coreConfigIO frameworkCfgPath
  mbc ← readMBotConfig mBotConfigFile
  mb ← buildMakerBot mbc
  di ← getDexInfo mbc

  let netId = cfgNetworkId coreCfg
  withCfgProviders coreCfg "" $ \providers →
    case action of
      "Run" → do
        c ← connectDB netId providers
        pp ← buildPP c di (mbcPriceConfig mbc)
        executeStrategy (fixedSpreadVsMarketPriceStrategy (mbcStrategyConfig mbc)) mb netId providers pp di
      "Cancel" → cancelAllOrders mb netId providers di
      _ → throwIO . userError $ "Action '" ++ action ++ "' not supported. Check cli arguments."
