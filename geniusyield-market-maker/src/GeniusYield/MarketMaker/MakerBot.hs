module GeniusYield.MarketMaker.MakerBot where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (displayException), Handler (Handler), catches)
import Control.Monad (forM_, forever, when)
import Control.Monad.Reader (runReaderT)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M
import GeniusYield.Api.Dex.PartialOrder (
  PartialOrderInfo (poiOwnerKey),
  cancelMultiplePartialOrders,
  partialOrders,
  placePartialOrder,
 )
import GeniusYield.Imports (printf)
import GeniusYield.MarketMaker.Constants (awaitTxParams, logNS)
import GeniusYield.MarketMaker.Prices
import GeniusYield.MarketMaker.Strategies
import GeniusYield.MarketMaker.Utils (
  DEXInfo (dexPORefs),
  addrFromSkey,
  pkhFromSkey,
 )
import GeniusYield.Providers.Common (SubmitTxException)
import GeniusYield.Transaction (BuildTxException)
import GeniusYield.TxBuilder
import GeniusYield.Types
import System.Exit

data MakerBot = MakerBot
  { -- | User.
    mbUser ∷ User,
    -- | Delay in microseconds between each iteration of execution strategy loop.
    mbDelay ∷ Int,
    -- | Non-ada token as other pair of the token is assumed to be ada.
    mbToken ∷ MMToken
  }

-----------------------------------------------------------------------
---------------------------- ACTIONS ----------------------------------

-- | Scan the chain for existing orders and cancel all of them in batches of 6.
cancelAllOrders ∷ MakerBot → GYNetworkId → GYProviders → DEXInfo → IO ()
cancelAllOrders MakerBot {mbUser} netId providers di = do
  let go ∷ [PartialOrderInfo] → IO ()
      go partialOrderInfos = do
        gyLogInfo providers logNS $ "---------- " ++ show (length partialOrderInfos) ++ " orders to cancel! -----------"
        when (null partialOrderInfos) $ do
          gyLogInfo providers logNS "---------- No more orders to cancel! -----------"
          exitSuccess
        let (batch, rest) = splitAt 6 partialOrderInfos
            userAddr = addrFromSkey netId $ uSKey mbUser
        txBody ←
          runGYTxMonadNode netId providers [userAddr] userAddr (uColl mbUser)
            $ runReaderT (cancelMultiplePartialOrders (dexPORefs di) batch) di
        let signedTx =
              signGYTxBody txBody [uSKey mbUser]
        tid ← gySubmitTx providers signedTx
        gyLogInfo providers logNS $ "Submitted a cancel order batch: " ++ show tid
        gyLogInfo providers logNS "---------- Done for the block! -----------"
        gyAwaitTxConfirmed providers awaitTxParams tid
        go rest
  partialOrderInfos ← runGYTxQueryMonadNode netId providers $ runReaderT (partialOrders (dexPORefs di)) di
  let userPkh = pkhFromSkey . uSKey $ mbUser
      userPOIs = filter (\o → poiOwnerKey o == userPkh) $ M.elems partialOrderInfos
  go userPOIs

buildAndSubmitActions ∷ User → GYProviders → GYNetworkId → UserActions → DEXInfo → IO ()
buildAndSubmitActions User {uSKey, uColl} providers netId ua di = flip catches handlers $ do
  let userAddr = addrFromSkey netId uSKey
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
          Nothing
    buildCommon txBody
 where
  logWarn = gyLogWarning providers logNS
  logInfo = gyLogInfo providers logNS

  handlers =
    let handlerCommon ∷ Exception e ⇒ e → IO ()
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
    let tx = signGYTxBody txBody [uSKey]
    tid ← gySubmitTx providers tx
    let numConfirms = confirmations awaitTxParams
    logInfo $ printf "Successfully submitted above tx, now waiting for %d confirmation(s)" numConfirms
    gyAwaitTxConfirmed providers awaitTxParams tid
    logInfo $ printf "Tx successfully seen on chain with %d confirmation(s)" numConfirms

executeStrategy
  ∷ Strategy
  → MakerBot
  → GYNetworkId
  → GYProviders
  → PricesProviders
  → DEXInfo
  → IO ()
executeStrategy runStrategy MakerBot {mbUser, mbDelay, mbToken} netId providers pp di =
  forever $ do
    newActions ← runStrategy pp mbUser mbToken

    buildAndSubmitActions mbUser providers netId newActions di

    gyLogInfo providers logNS "---------- Done for the block! -----------"
    threadDelay mbDelay
