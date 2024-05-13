module GeniusYield.Test.MarketMaker.MakerBot where

import qualified Data.List.NonEmpty                 as NE (toList)
import           Control.Concurrent.MVar
import           Control.Monad                      (zipWithM_)
import           Control.Monad.State                (StateT (..), get, lift)
import           Data.Maybe                         (catMaybes)
import           GeniusYield.Api.Dex.Constants      (DEXInfo (..))
import           GeniusYield.MarketMaker.MakerBot   (MakerBot(..), MBFret(..), mbStateMachine)
import           GeniusYield.MarketMaker.Prices
import           GeniusYield.MarketMaker.Strategies
import           GeniusYield.Test.MarketMaker.Utils
import           GeniusYield.Types


getMockMVars :: PricesProviders -> [MVar (Maybe Double)]
getMockMVars pp = map mokppPrice mces
  where
    mces = catMaybes $ maybeMock <$> (NE.toList . ppaPricesCluster . pricesAggregatorPP $ pp)
    maybeMock :: PricesProviderBuilt -> Maybe MockConfigExtended
    maybeMock ppb = case ppb of
      MockPPB mce -> Just mce
      _           -> Nothing

updateMockVars :: [MVar (Maybe Double)] -> PPStatus -> IO ()
updateMockVars mvars (PPStatus mbps) = zipWithM_ update mvars mbps
  where
    update mvar mbp = modifyMVar_ mvar $ \_ -> pure mbp

evolveStrategy
  :: Strategy
  -> MakerBot
  -> GYNetworkId
  -> GYProviders
  -> PricesProviders
  -> DEXInfo
  -> MVar [LogData]
  -> StateT MBFret IO ()
evolveStrategy runStrategy mb netId providers pp di logRef = do
  mbTest $ \ppStatus -> do
    lift $ modifyMVar_ logRef $ \current -> pure $ current ++ [LDStatus ppStatus]
    lift $ updateMockVars (getMockMVars pp) ppStatus
    
    get >>= mbStateMachine runStrategy mb netId providers pp di
       
executeStrategy
  :: Strategy
  -> MakerBot
  -> GYNetworkId
  -> GYProviders
  -> PricesProviders
  -> DEXInfo
  -> MVar [LogData]
  -> IO ()
executeStrategy runStrategy mb netId providers pp di logRef = do
  runStateT
    (evolveStrategy runStrategy mb netId providers pp di logRef)
    MBReady
  return ()
