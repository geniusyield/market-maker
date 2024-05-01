module GeniusYield.MarketMaker.TestUtils where

import           Data.List                         (isInfixOf, findIndex)
import           Control.Concurrent.MVar
import           Control.Monad                     (foldM)
import           Control.Monad.State               (StateT)
import           GeniusYield.MarketMaker.Constants (logNS)
import           GeniusYield.Types


data PPStatus = PPStatus { maestroOffset :: Double, maestroAvailable :: Bool, taptoolsAvailable :: Bool }
  deriving stock Show

normalPPStatus :: PPStatus
normalPPStatus = PPStatus { maestroOffset = 0, maestroAvailable = True, taptoolsAvailable = True }

newtype MBLog = MBLog (GYLogSeverity, GYLogNamespace, String)
  deriving newtype Show

data LogData = LDStatus PPStatus | LDLog MBLog
  deriving stock Show

augmentedLogRun :: MVar [LogData]
                -> (GYLogNamespace -> GYLogSeverity -> String -> IO ())
                -> GYLogNamespace -> GYLogSeverity -> String
                -> IO ()
augmentedLogRun logRef f ns s msg = do
  modifyMVar_ logRef $ \current -> pure $ current ++ [LDLog (MBLog (s, ns, msg))]

  logRef' <- readMVar logRef
  writeFile "./mmbot.log" (show logRef')
  
  f ns s msg

mbTest :: forall a. (PPStatus -> StateT a IO ()) -> StateT a IO ()
mbTest f = foldM (const f) () testSequence

examineLog :: [LogData] -> Bool
examineLog = areEventsOrdered testRequirements 


-------------------------------------------------------------------------------
-- Test Sequence & Requirements
-------------------------------------------------------------------------------

testSequence :: [PPStatus]
testSequence = [ normalPPStatus,
                 PPStatus 0 False True,                           -- Maestro fails
                 PPStatus 0.1 True True,                          -- Maestro recovers but with outrageous price mismatch
                 normalPPStatus, normalPPStatus, normalPPStatus,  -- Price agreement back to normal for a short time
                 PPStatus 0.1 True True,                          -- Price mismatch again
                 normalPPStatus, normalPPStatus, normalPPStatus,  -- Price agreement back to normal, for a long enough time
                 normalPPStatus, normalPPStatus, normalPPStatus,
                 normalPPStatus, normalPPStatus, normalPPStatus,
                 normalPPStatus, normalPPStatus, normalPPStatus   -- Having waited enough, MMBot should resume strategy
               ]

testRequirements :: [[LogData -> Bool]]
testRequirements =
  [ [ isPPStatusNormal, isMBLogInfo msgFinishedStrategy, isMBLogInfo msgTxOnChain, isMBLogInfo msgDoneForTheBlock ]
  , [ isPPStatus (0, False, True), isMBLogWarning msgOnePProviderFailed, isMBLogInfo msgFinishedStrategy, isMBLogInfo msgDoneForTheBlock ]
  , [ isPPStatus (0.1, True, True), isMBLogWarning msgOutrageousMismatch, isMBLogInfo msgCancelOrderBatch, isMBLogInfo msgNoMoreOrdersToCancel ]
  , [ isPPStatusNormal, isMBLogInfo msgApparentRecovery ]
  , [ isPPStatus (0.1, True, True), isMBLogWarning msgOutrageousPersists ]
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
msgOnePProviderFailed = "One prices provider failed"
msgOutrageousMismatch = "Closed all orders due to: outrageous price mismatch among Prices Providers"
msgOutrageousPersists = "Outrageous price mismatch persist"
msgCancelOrderBatch = "Submitted a cancel order batch"
msgNoMoreOrdersToCancel = "No more orders to cancel!"
msgApparentRecovery = "Apparent recovery of Prices Providers"
msgResumingStrategy = "Resuming strategy"


-------------------------------------------------------------------------------
-- Helper functions
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

isPPStatus :: (Double, Bool, Bool) -> LogData -> Bool
isPPStatus (r, bM, bT) ld = case ld of
  LDStatus (PPStatus r' bM' bT') -> r == r' && bM == bM' && bT == bT'
  _                              -> False

isPPStatusNormal :: LogData -> Bool
isPPStatusNormal = isPPStatus (0, True, True)

isMBLog :: (GYLogSeverity, String) -> LogData -> Bool
isMBLog (s, msg) ld = case ld of
  LDLog (MBLog (s', ns, msg')) -> s == s' && ns == logNS && isInfixOf msg msg'
  _                            -> False

isMBLogInfo, isMBLogWarning :: String -> LogData -> Bool
isMBLogInfo msg    = isMBLog (GYInfo, msg)
isMBLogWarning msg = isMBLog (GYWarning, msg)
