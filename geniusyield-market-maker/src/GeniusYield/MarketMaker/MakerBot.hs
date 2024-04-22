module GeniusYield.MarketMaker.MakerBot where

import           Control.Concurrent                 (threadDelay)
import           Control.Exception                  (Exception (displayException),
                                                     Handler (Handler), catches)
import           Control.Monad                      (forM_, forever)
import           Control.Monad.Reader               (runReaderT)
import           Control.Monad.State                (StateT (..), get, put, lift)
import           Data.List.Split                    (chunksOf)
import qualified Data.Map.Strict                    as M
import           GeniusYield.Api.Dex.PartialOrder   (PartialOrderInfo (poiOwnerKey),
                                                     cancelMultiplePartialOrders,
                                                     partialOrders,
                                                     placePartialOrder)
import           GeniusYield.Imports                (printf, (&))  -- TODO: add 'fromMaybe'?
import           GeniusYield.MarketMaker.Constants  (awaitTxParams, logNS)
import           GeniusYield.MarketMaker.Prices
import           GeniusYield.MarketMaker.Strategies
import           GeniusYield.MarketMaker.User
import           GeniusYield.MarketMaker.Utils      (DEXInfo (dexPORefs),
                                                     addrUser, pkhUser)
import           GeniusYield.Providers.Common       (SubmitTxException)
import           GeniusYield.Transaction            (BuildTxException)
import           GeniusYield.TxBuilder
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
                        { mbsRelax1  ∷ !Int
                        , mbsWorse1  ∷ !Int
                        }
                      | MBSpooked2
                        { mbsRelax2 ∷ !Int }
                          

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
                $ runReaderT (cancelMultiplePartialOrders (dexPORefs di) batch) di
            let signedTx =
                  signGYTxBody' txBody [uSKeyToSomeSigningKey mbUser]
            tid ← gySubmitTx providers signedTx
            gyLogInfo providers logNS $ "Submitted a cancel order batch: " ++ show tid
            gyLogInfo providers logNS "---------- Done for the block! -----------"
            gyAwaitTxConfirmed providers awaitTxParams tid
            go rest
  partialOrderInfos ← runGYTxQueryMonadNode netId providers $ runReaderT (partialOrders (dexPORefs di)) di
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
    txBody ← runGYTxMonadNode netId providers [userAddr] userAddr uColl $ flip runReaderT di $ cancelMultiplePartialOrders (dexPORefs di) (map coaPoi cancelChunk)
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

        be ∷ BuildTxException → IO ()
        be = handlerCommon

        se ∷ SubmitTxException → IO ()
        se = handlerCommon

        me ∷ GYTxMonadException → IO ()
        me = handlerCommon
     in [Handler be, Handler se, Handler me]

  buildCommon txBody = do
    logInfo $ "Successfully built body for above action, tx id: " <> show (txBodyTxId txBody)
    let tx = signGYTxBody' txBody [uSKeyToSomeSigningKey user]
    tid ← gySubmitTx providers tx
    let numConfirms = confirmations awaitTxParams
    logInfo $ printf "Successfully submitted above tx, now waiting for %d confirmation(s)" numConfirms
    gyAwaitTxConfirmed providers awaitTxParams tid
    logInfo $ printf "Tx successfully seen on chain with %d confirmation(s)" numConfirms

evolveStrategy
  ∷ Strategy
  → MakerBot
  → GYNetworkId
  → GYProviders
  → PricesProviders
  → DEXInfo
  → StateT MBFret IO ()
evolveStrategy runStrategy mb@MakerBot {mbUser, mbDelay, mbToken} netId providers pp di = do
  let cfg          = ppaCommonCfg . pricesAggregatorPP $ pp
      respiteDelay = ceiling $ pccRespiteDelayFactor cfg * fromIntegral mbDelay
  
  forever $ do
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
            put MBSpooked1 { mbsRelax1 = 0, mbsWorse1 = 0 }
            lift $ gyLogInfo providers logNS "Closed all orders due to price mismatch among Prices Providers"

            lift $ cancelAllOrders' mb netId providers di
            lift $ threadDelay respiteDelay

          UACSpooked2 e → do
            put MBSpooked2 { mbsRelax2 = 0 }
            lift $ gyLogWarning providers logNS $ "Closed all orders due to: " ++ e

            lift $ cancelAllOrders' mb netId providers di
            lift $ threadDelay respiteDelay

      MBSpooked1 {..} → do
        if mbsRelax1 > pccAfterExitRelaxAim1 cfg then do
             lift $ gyLogInfo providers logNS $ "Resuming strategy"
             put MBReady
           else if mbsWorse1 > pccAfterExitWorseMax1 cfg
                   then do
                     lift $ gyLogWarning providers logNS $ "Price mismatch among providers lasting a long time!"
                     put MBSpooked2 { mbsRelax2 = 0 }
                   else do
                     pe ← lift $ priceEstimate' pp mbToken

                     case pe of
                       PriceMismatch1    → do
                         lift $ gyLogInfo providers logNS $ "Price mismatch persists"
                         put MBSpooked1 { mbsRelax1 = 0, mbsWorse1 = mbsWorse1 + 1 }

                       PriceMismatch2    → do
                         lift $ gyLogWarning providers logNS $ "Outrageous price mismatch among providers!"
                         put MBSpooked2 { mbsRelax2 = 0 }

                       PriceUnavailable  → do
                         lift $ gyLogWarning providers logNS $ "All Prices Providers unavailable!"
                         put MBSpooked2 { mbsRelax2 = 0 }

                       PriceSourceFail _ → do
                         lift $ gyLogInfo providers logNS $ "One prices provider has failed"

                       _                 → do
                         lift $ gyLogInfo providers logNS $ "Apparent recovery of Prices Providers; waiting for relaxation period to elapse."
                         put MBSpooked1 { mbsRelax1 = mbsRelax1 + 1, mbsWorse1 = mbsWorse1 }

                     lift $ threadDelay respiteDelay

      MBSpooked2 {..} → do
        if mbsRelax2 > pccAfterExitRelaxAim2 cfg then do
             lift $ gyLogInfo providers logNS $ "Resuming strategy"
             put MBReady
           else do
             pe ← lift $ priceEstimate' pp mbToken

             case pe of
               PriceMismatch1    → do
                 lift $ gyLogWarning providers logNS $ "Price mismatch persists"
                 put MBSpooked2 { mbsRelax2 = 0 }

               PriceMismatch2    → do
                 lift $ gyLogWarning providers logNS $ "Outrageous price mismatch persists"
                 put MBSpooked2 { mbsRelax2 = 0 }

               PriceUnavailable  → do
                 lift $ gyLogWarning providers logNS $ "All Prices Providers unavailable"
                 put MBSpooked2 { mbsRelax2 = 0 }

               PriceSourceFail _ → do
                 lift $ gyLogInfo providers logNS $ "One prices provider has failed"

               _                 → do
                 lift $ gyLogInfo providers logNS $ "Apparent recovery of Prices Providers; waiting for relaxation period to elapse."
                 put MBSpooked2 { mbsRelax2 = mbsRelax2 + 1 }

             lift $ threadDelay respiteDelay
        
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
