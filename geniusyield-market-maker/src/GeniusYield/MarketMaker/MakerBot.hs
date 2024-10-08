module GeniusYield.MarketMaker.MakerBot where

import           Control.Concurrent                 (threadDelay)
import           Control.Exception                  (Exception (displayException),
                                                     Handler (Handler), catches)
import           Control.Monad                      (forM_, forever)
import           Control.Monad.Reader               (runReaderT)
import           Control.Monad.State                (StateT (..), get, lift,
                                                     put)
import           Data.List.Split                    (chunksOf)
import qualified Data.Map.Strict                    as M
import           GeniusYield.Api.Dex.Constants      (DEXInfo (..))
import           GeniusYield.Api.Dex.PartialOrder   (PartialOrderInfo (poiOwnerKey),
                                                     cancelMultiplePartialOrders',
                                                     partialOrders,
                                                     placePartialOrder)
import           GeniusYield.Imports                (fromMaybe, printf, (&))
import           GeniusYield.MarketMaker.Constants  (awaitTxParams, logNS)
import           GeniusYield.MarketMaker.Prices
import           GeniusYield.MarketMaker.Strategies
import           GeniusYield.MarketMaker.User
import           GeniusYield.MarketMaker.Utils      (addrUser, pkhUser)
import           GeniusYield.Providers.Common       (SubmitTxException)
import           GeniusYield.TxBuilder              hiding (User)
import           GeniusYield.Types
import           System.Exit

data MakerBot = MakerBot
  { -- | User.
    mbUser  ∷ User,
    -- | Delay in microseconds between each iteration of execution strategy loop.
    mbDelay ∷ Int,
    -- | Non-ADA token as other pair of the token is assumed to be ADA.
    mbToken ∷ MMToken
  }

data MBFret = MBReady | MBSpooked1
                        { mbs1Relax ∷ !Int
                        , mbs1Worse ∷ !Int
                        }
                      | MBSpooked2
                        { mbs2Relax ∷ !Int }


-----------------------------------------------------------------------
---------------------------- ACTIONS ----------------------------------

-- | Scan the chain for existing orders and cancel all of them in batches of 6, then exit process.
cancelAllOrders ∷ MakerBot → GYNetworkId → GYProviders → DEXInfo → IO ()
cancelAllOrders mb netId providers di = do
  cancelAllOrders' mb netId providers di
  exitSuccess

-- | Scan the chain for existing orders and cancel all of them in batches of 6.
cancelAllOrders' ∷ MakerBot → GYNetworkId → GYProviders → DEXInfo → IO ()
cancelAllOrders' MakerBot {mbUser} netId providers di = do
  let go ∷ [PartialOrderInfo] → IO ()
      go partialOrderInfos = do
        gyLogInfo providers logNS $ "---------- " ++ show (length partialOrderInfos) ++ " orders to cancel! -----------"
        if null partialOrderInfos
          then gyLogInfo providers logNS "---------- No more orders to cancel! -----------"
          else do
            let (batch, rest) = splitAt 6 partialOrderInfos
                userAddr = addrUser netId mbUser
            txBody ←
              runGYTxMonadNode netId providers [userAddr] userAddr (uColl mbUser)
                $ runReaderT (cancelMultiplePartialOrders' (dexPORefs di) batch) di
            let signedTx =
                  signGYTxBody' txBody [uSKeyToSomeSigningKey mbUser]
            tid ← gySubmitTx providers signedTx
            gyLogInfo providers logNS $ "Submitted a cancel order batch: " ++ show tid
            gyLogInfo providers logNS "---------- Done for the block! -----------"
            gyAwaitTxConfirmed providers awaitTxParams tid
            go rest
  partialOrderInfos ← runGYTxQueryMonadIO netId providers $ runReaderT (partialOrders (dexPORefs di)) di
  let userPkh = pkhUser mbUser & toPubKeyHash
      userPOIs = filter (\o → poiOwnerKey o == userPkh) $ M.elems partialOrderInfos
  go userPOIs

buildAndSubmitActions ∷ User → GYProviders → GYNetworkId → UserActions → DEXInfo → IO ()
buildAndSubmitActions user@User {uColl, uStakeCred} providers netId ua di = flip catches handlers $ do
  let userAddr = addrUser netId user
      placeActions = uaPlaces ua
      cancelActions = uaCancels ua

  forM_ (chunksOf 6 cancelActions) $ \cancelChunk → do
    logInfo $ "Building for cancel action(s): " <> show cancelChunk
    txBody ← runGYTxMonadNode netId providers [userAddr] userAddr uColl $ flip runReaderT di $ cancelMultiplePartialOrders' (dexPORefs di) (map coaPoi cancelChunk)
    buildCommon txBody

  forM_ placeActions $ \pa@PlaceOrderAction {..} → do
    logInfo $ "Building for place action: " <> show pa
    txBody ←
      runGYTxMonadNode netId providers [userAddr] userAddr uColl
        $ flip runReaderT di
        $ placePartialOrder
          (dexPORefs di)
          userAddr
          (poaOfferedAmount, poaOfferedAsset)
          poaAskedAsset
          poaPrice
          Nothing
          Nothing
          uStakeCred
    buildCommon txBody
 where
  logWarn = gyLogWarning providers logNS
  logInfo = gyLogInfo providers logNS

  handlers =
    let handlerCommon ∷ Exception e => e → IO ()
        handlerCommon = logWarn . displayException

        se ∷ SubmitTxException → IO ()
        se = handlerCommon

        me ∷ GYTxMonadException → IO ()
        me = handlerCommon
     in [Handler se, Handler me]

  buildCommon txBody = do
    logInfo $ "Successfully built body for above action, tx id: " <> show (txBodyTxId txBody)
    let tx = signGYTxBody' txBody [uSKeyToSomeSigningKey user]
    tid ← gySubmitTx providers tx
    let numConfirms = confirmations awaitTxParams
    logInfo $ printf "Successfully submitted above tx, now waiting for %d confirmation(s)" numConfirms
    gyAwaitTxConfirmed providers awaitTxParams tid
    logInfo $ printf "Tx successfully seen on chain with %d confirmation(s)" numConfirms


-----------------------------------------------------------------------
-------------------------- STATE MACHINE ------------------------------

mbStateMachine ∷ Strategy
               → MakerBot
               → GYNetworkId
               → GYProviders
               → PricesProviders
               → DEXInfo
               → StateT MBFret IO ()
mbStateMachine runStrategy mb@MakerBot {mbUser, mbDelay, mbToken} netId providers pp di = do
  let cfg          = ppaCommonCfg . pricesAggregatorPP $ pp
      respiteDelay = ceiling $ fromMaybe 1 (pccRespiteDelayFactor cfg) * fromIntegral mbDelay

  mbFret ← get

  case mbFret of
    MBReady → do
      (newActions, controller) ← lift $ runStrategy pp mbUser mbToken
      case controller of
        UACNormal     → do
          lift $ buildAndSubmitActions mbUser providers netId newActions di

          lift $ gyLogInfo providers logNS "---------- Done for the block! -----------"
          lift $ threadDelay mbDelay

        UACSpooked1   → do
          put MBSpooked1 { mbs1Relax = 0, mbs1Worse = 0 }
          lift $ gyLogInfo providers logNS "Closing all orders due to price mismatch among Prices Providers"

          lift $ cancelAllOrders' mb netId providers di
          lift $ threadDelay respiteDelay

        UACSpooked2 e → do
          put MBSpooked2 { mbs2Relax = 0 }
          lift $ gyLogWarning providers logNS $ "Closing all orders due to: " ++ e

          lift $ cancelAllOrders' mb netId providers di
          lift $ threadDelay respiteDelay

    MBSpooked1 {..} → do
      if mbs1Relax >= pccAfterExitRelaxAim1 cfg then do
           lift $ gyLogInfo providers logNS "[MBSpooked1] Resuming strategy"
           put MBReady
         else if mbs1Worse >= pccAfterExitWorseMax1 cfg
                 then do
                   lift $ gyLogWarning providers logNS "[MBSpooked1] Price mismatch among providers lasting a long time!"
                   put MBSpooked2 { mbs2Relax = 0 }
                 else do
                   pe ← lift $ priceEstimate' pp mbToken

                   case pe of
                     PriceMismatch1           → do
                       lift $ gyLogInfo providers logNS "[MBSpooked1] Price mismatch persists"
                       put MBSpooked1 { mbs1Relax = 0, mbs1Worse = mbs1Worse + 1 }

                     PriceMismatch2           → do
                       lift $ gyLogWarning providers logNS "[MBSpooked1] Outrageous price mismatch among providers!"
                       put MBSpooked2 { mbs2Relax = 0 }

                     PriceUnavailable         → do
                       lift $ gyLogWarning providers logNS "[MBSpooked1] All Prices Providers unavailable!"
                       put MBSpooked2 { mbs2Relax = 0 }

                     PriceSourceFail es pps _ → lift $ logPricesProviderFail providers es pps

                     PriceAverage _           → do
                       lift $ gyLogInfo providers logNS "[MBSpooked1] Apparent recovery of Prices Providers; waiting for observation period to elapse."
                       put MBSpooked1 { mbs1Relax = mbs1Relax + 1, mbs1Worse = mbs1Worse }

                   lift $ threadDelay respiteDelay

    MBSpooked2 {..} → do
      if mbs2Relax >= pccAfterExitRelaxAim2 cfg then do
           lift $ gyLogInfo providers logNS "[MBSpooked2] Resuming strategy"
           put MBReady
         else do
           pe ← lift $ priceEstimate' pp mbToken

           case pe of
             PriceMismatch1           → do
               lift $ gyLogWarning providers logNS "[MBSpooked2] Price mismatch persists"
               put MBSpooked2 { mbs2Relax = 0 }

             PriceMismatch2           → do
               lift $ gyLogWarning providers logNS "[MBSpooked2] Outrageous price mismatch persists"
               put MBSpooked2 { mbs2Relax = 0 }

             PriceUnavailable         → do
               lift $ gyLogWarning providers logNS "[MBSpooked2] All Prices Providers unavailable"
               put MBSpooked2 { mbs2Relax = 0 }

             PriceSourceFail es pps _ → lift $ logPricesProviderFail providers es pps

             PriceAverage _           → do
               lift $ gyLogInfo providers logNS "[MBSpooked2] Apparent recovery of Prices Providers; waiting for observation period to elapse."
               put MBSpooked2 { mbs2Relax = mbs2Relax + 1 }

           lift $ threadDelay respiteDelay

evolveStrategy
  ∷ Strategy
  → MakerBot
  → GYNetworkId
  → GYProviders
  → PricesProviders
  → DEXInfo
  → StateT MBFret IO ()
evolveStrategy runStrategy mb netId providers pp di =
  forever $ mbStateMachine runStrategy mb netId providers pp di

executeStrategy
  ∷ Strategy
  → MakerBot
  → GYNetworkId
  → GYProviders
  → PricesProviders
  → DEXInfo
  → IO ()
executeStrategy runStrategy mb netId providers pp di = do
  runStateT
    (evolveStrategy runStrategy mb netId providers pp di)
    MBReady
  return ()

runGYTxMonadNode :: GYNetworkId -> GYProviders -> [GYAddress] -> GYAddress -> Maybe (GYTxOutRef, Bool) -> GYTxBuilderMonadIO (GYTxSkeleton v) -> IO GYTxBody
runGYTxMonadNode nid providers addrs change collateral act = runGYTxBuilderMonadIO nid providers addrs change collateral $ act >>= buildTxBody
