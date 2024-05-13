module GeniusYield.Test.MarketMaker.Utils where

import           Data.List                         (isInfixOf, findIndex)
import           Control.Concurrent.MVar
import           Control.Monad.State               (StateT)
import           GeniusYield.MarketMaker.Constants (logNS)
import           GeniusYield.Types


data PPStatus = PPStatus [Maybe Double]
  deriving stock (Eq, Show)

mockPrice :: Double
mockPrice = 0.135

totalMocks :: Int
totalMocks = 2

normalPPStatus :: PPStatus
normalPPStatus = PPStatus $ replicate totalMocks (Just mockPrice)

newtype MBLog = MBLog (GYLogSeverity, GYLogNamespace, String)
  deriving newtype Show

data LogData = LDStatus PPStatus | LDLog MBLog
  deriving stock Show

augmentedLogRun :: MVar [LogData]
                -> (GYLogNamespace -> GYLogSeverity -> String -> IO ())
                -> GYLogNamespace -> GYLogSeverity -> String
                -> IO ()
augmentedLogRun logRef f ns s msg = do
  modifyMVar_ logRef $ \current -> pure $ LDLog (MBLog (s, ns, msg)) : current
  f ns s msg

mbTest :: forall a. (PPStatus -> StateT a IO ()) -> StateT a IO ()
mbTest f = mapM_ f testSequence

examineLog :: [LogData] -> Bool
examineLog = areEventsOrdered testRequirements


-------------------------------------------------------------------------------
-- Test Sequence & Requirements
-------------------------------------------------------------------------------

-- | This test assumes that, previously, all orders have been cancelled.
testSequence :: [PPStatus]
testSequence = [ normalPPStatus,
                 PPStatus [Nothing, Just mockPrice],                 -- Maestro fails
                 PPStatus [Just (mockPrice * 1.3), Just mockPrice],  -- Maestro recovers but with outrageous price mismatch
                 normalPPStatus, normalPPStatus, normalPPStatus,     -- Price agreement back to normal for a short time
                 PPStatus [Just (mockPrice * 1.3), Just mockPrice],  -- Price mismatch again
                 normalPPStatus, normalPPStatus, normalPPStatus,     -- Price agreement back to normal, for a long enough time
                 normalPPStatus, normalPPStatus, normalPPStatus,
                 normalPPStatus, normalPPStatus, normalPPStatus,
                 normalPPStatus, normalPPStatus, normalPPStatus      -- Having waited enough, MMBot should resume strategy
               ]

type LoggedEvent = [LogData -> Bool]

testRequirements :: [LoggedEvent]
testRequirements =
  [ [ isPPStatusNormal, isMBLogInfo msgFinishedStrategy, isMBLogInfo msgTxOnChain, isMBLogInfo msgDoneForTheBlock ]
  , [ isPPStatus [Nothing, Just mockPrice], isMBLogWarning msgOnePProviderFailed, isMBLogInfo msgFinishedStrategy, isMBLogInfo msgDoneForTheBlock ]
  , [ isPPStatus [Just (mockPrice * 1.3), Just mockPrice], isMBLogWarning msgOutrageousMismatch, isMBLogInfo msgCancelOrderBatch, isMBLogInfo msgNoMoreOrdersToCancel ]
  , [ isPPStatusNormal, isMBLogInfo msgApparentRecovery ]
  , [ isPPStatus [Just (mockPrice * 1.3), Just mockPrice], isMBLogWarning msgOutrageousPersists ]
  , [ isPPStatusNormal, isMBLogInfo msgApparentRecovery, isMBLogInfo msgResumingStrategy, isMBLogInfo msgFinishedStrategy,
      isMBLogInfo msgTxOnChain, isMBLogInfo msgDoneForTheBlock ]
  ]


-------------------------------------------------------------------------------
-- Messages
-------------------------------------------------------------------------------

msgFinishedStrategy, msgTxOnChain, msgDoneForTheBlock                                  :: String
msgOnePProviderFailed, msgOutrageousMismatch, msgOutrageousPersists                    :: String
msgCancelOrderBatch, msgNoMoreOrdersToCancel, msgApparentRecovery, msgResumingStrategy :: String

msgFinishedStrategy = "FINISHED STRATEGY"
msgTxOnChain = "Tx successfully seen on chain"
msgDoneForTheBlock = "Done for the block!"
msgOnePProviderFailed = "Some prices provider(s) failed"
msgOutrageousMismatch = "Closing all orders due to: outrageous price mismatch among Prices Providers"
msgOutrageousPersists = "Outrageous price mismatch persists"
msgCancelOrderBatch = "Submitted a cancel order batch"
msgNoMoreOrdersToCancel = "No more orders to cancel!"
msgApparentRecovery = "Apparent recovery of Prices Providers"
msgResumingStrategy = "Resuming strategy"


-------------------------------------------------------------------------------
-- Testing helper functions
-------------------------------------------------------------------------------

isOrdered :: [Int] -> Bool
isOrdered [] = True
isOrdered ns = and $ zipWith (<=) ns (tail ns)

arePredicatesOrdered :: [a -> Bool] -> [a] -> (Bool, Int)
arePredicatesOrdered ps xs = case midxs of
    Nothing   -> (False, 0)
    Just idxs -> (isOrdered idxs, last idxs)
  where
    midxs = sequenceA $ flip findIndex xs <$> ps

areEventsOrdered :: [[a -> Bool]] -> [a] -> Bool
areEventsOrdered [] _        = True
areEventsOrdered (ps:pss) xs =
  let (b, i) = arePredicatesOrdered ps xs
  in  if b then areEventsOrdered pss (drop (i + 1) xs) else False

isPPStatus :: [Maybe Double] -> LogData -> Bool
isPPStatus mbps ld = case ld of
  LDStatus (PPStatus mbps') -> mbps == mbps'
  _                         -> False

isPPStatusNormal :: LogData -> Bool
isPPStatusNormal = isPPStatus $ replicate totalMocks (Just mockPrice)

isMBLog :: (GYLogSeverity, String) -> LogData -> Bool
isMBLog (s, msg) ld = case ld of
  LDLog (MBLog (s', ns, msg')) -> s == s' && ns == logNS && isInfixOf msg msg'
  _                            -> False

isMBLogInfo, isMBLogWarning :: String -> LogData -> Bool
isMBLogInfo msg    = isMBLog (GYInfo, msg)
isMBLogWarning msg = isMBLog (GYWarning, msg)
