module GeniusYield.Test.MarketMaker.MakerBot where

import           Control.Concurrent.MVar
import           Control.Monad.State                (StateT (..), get, lift)
import           Data.Maybe                         (fromJust)
import           GeniusYield.Api.Dex.Constants      (DEXInfo (..))
import           GeniusYield.MarketMaker.MakerBot   (MakerBot(..), MBFret(..), mbStateMachine)
import           GeniusYield.MarketMaker.Prices
import           GeniusYield.MarketMaker.Strategies
import           GeniusYield.Test.MarketMaker.Utils
import           GeniusYield.Types


evolveStrategy
  ∷ Strategy
  → MakerBot
  → GYNetworkId
  → GYProviders
  → PricesProviders
  → DEXInfo
  → MVar [LogData]
  → StateT MBFret IO ()
evolveStrategy runStrategy mb netId providers pp di logRef = do
  let cluster  = ppaPricesCluster . pricesAggregatorPP $ pp

  mock1Cfg ← lift . getMock $ cluster!!0
  mock2Cfg ← lift . getMock $ cluster!!1

  let path1Offset  = fromJust $ mokcOffsetPath mock1Cfg
      path1Avail   = fromJust $ mokcAvailablePath mock1Cfg
      path2Avail   = fromJust $ mokcAvailablePath mock2Cfg
      flagPPStatus = writeFlags path1Offset path1Avail path2Avail

  mbTest $ \ppStatus → do
    lift $ modifyMVar_ logRef $ \current → pure $ current ++ [LDStatus ppStatus]
    lift $ flagPPStatus ppStatus
    
    get >>= mbStateMachine runStrategy mb netId providers pp di
       
executeStrategy
  ∷ Strategy
  → MakerBot
  → GYNetworkId
  → GYProviders
  → PricesProviders
  → DEXInfo
  → MVar [LogData]
  → IO ()
executeStrategy runStrategy mb netId providers pp di logRef = do
  runStateT
    (evolveStrategy runStrategy mb netId providers pp di logRef)
    MBReady
  return ()
