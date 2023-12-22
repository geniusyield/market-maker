module GeniusYield.MarketMaker.MakerBot where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, handle)
import Control.Monad (forever, liftM2, when)
import Control.Monad.Reader (runReaderT)
import Data.Functor.Identity (runIdentity)
import qualified Data.List.NonEmpty as NE (toList)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M
import GeniusYield.Api.Dex.PartialOrder (
  PartialOrderInfo (poiOwnerKey),
  cancelMultiplePartialOrders,
  partialOrders,
  placePartialOrder,
 )
import GeniusYield.MarketMaker.Prices
import GeniusYield.MarketMaker.Strategies
import GeniusYield.MarketMaker.Utils (
  DEXInfo (dexPORefs),
  addrFromSkey,
  pkhFromSkey,
 )
import GeniusYield.TxBuilder
import GeniusYield.Types
import System.Exit

data MakerBot = MakerBot
  { -- | User.
    mbUser ∷ User,
    -- | Delay in microseconds between each iteration of execution strategy loop.
    mbDelay ∷ Int,
    -- | Non-ada token as other pair of the token is assumed to be ada.
    mbToken ∷ SimToken
  }

-----------------------------------------------------------------------
---------------------------- ACTIONS ----------------------------------

-- | For each `PlaceOrderAction`, get a skeleton that places that order.
placeOrders ∷ GYNetworkId → User → [PlaceOrderAction] → DEXInfo → GYTxMonadNode [GYTxSkeleton 'PlutusV2]
placeOrders _ _ [] _ = return []
placeOrders netId User {uSKey} sts di = do
  let userAddr = addrFromSkey netId uSKey

  let getOrderSkeleton ∷ PlaceOrderAction → GYTxMonadNode (GYTxSkeleton 'PlutusV2)
      getOrderSkeleton PlaceOrderAction {..} =
        flip runReaderT di
          $ placePartialOrder
            (dexPORefs di)
            userAddr
            (poaOfferedAmount, poaOfferedAsset)
            poaAskedAsset
            poaPrice
            Nothing
            Nothing
            Nothing

  mapM getOrderSkeleton sts

-- | Returns a skeleton that cancels all orders from the given list.
cancelOrders ∷ [CancelOrderAction] → DEXInfo → GYTxMonadNode [GYTxSkeleton 'PlutusV2]
cancelOrders [] _ = return []
cancelOrders coas di = mapM (flip runReaderT di . cancelMultiplePartialOrders (dexPORefs di) . map coaPoi) $ chunksOf 6 coas

-- | Scan the chain for existing orders and cancel all of them in batches of 6.
cancelAllOrders ∷ MakerBot → GYNetworkId → GYProviders → DEXInfo → IO ()
cancelAllOrders MakerBot {mbUser} netId providers di = do
  let go ∷ [PartialOrderInfo] → IO ()
      go partialOrderInfos = do
        gyLogInfo providers "MM" $ "---------- " ++ show (length partialOrderInfos) ++ " orders to cancel! -----------"
        when (null partialOrderInfos) $ do
          gyLogInfo providers "MM" "---------- No more orders to cancel! -----------"
          exitSuccess
        let (batch, rest) = splitAt 6 partialOrderInfos
            userAddr = addrFromSkey netId $ uSKey mbUser
        txBody ←
          runGYTxMonadNode netId providers [userAddr] userAddr (uColl mbUser)
            $ runReaderT (cancelMultiplePartialOrders (dexPORefs di) batch) di
        let signedTx =
              signGYTxBody txBody [uSKey mbUser]
        tid ← gySubmitTx providers signedTx
        gyLogInfo providers "MM" $ "Submitted a cancel order batch: " ++ show tid
        gyLogInfo providers "MM" "---------- Done for the block! -----------"
        gyAwaitTxConfirmed providers (GYAwaitTxParameters {maxAttempts = 20, confirmations = 1, checkInterval = 20_000_000}) tid
        go rest
  partialOrderInfos ← runGYTxQueryMonadNode netId providers $ runReaderT (partialOrders (dexPORefs di)) di
  let userPkh = pkhFromSkey . uSKey $ mbUser
      userPOIs = filter (\o → poiOwnerKey o == userPkh) $ M.elems partialOrderInfos
  go userPOIs

signAndSubmit ∷ User → GYProviders → GYNetworkId → GYTxMonadNode [GYTxSkeleton 'PlutusV2] → IO ()
signAndSubmit User {uSKey, uColl} providers netId skeletons = handle hanldeSignAndSubmit $ do
  let userAddr = addrFromSkey netId uSKey

  txBodyRes ← runGYTxMonadNodeParallel netId providers [userAddr] userAddr uColl skeletons

  bodies ← case txBodyRes of
    GYTxBuildSuccess txs → return $ getBodies txs
    GYTxBuildPartialSuccess v txs →
      logWarn (unwords ["Partial Success:", show v])
        >> return (getBodies txs)
    GYTxBuildFailure v →
      logWarn (unwords ["Insufficient funds:", show v])
        >> return []
    GYTxBuildNoInputs → logWarn "No Inputs" >> return []

  let txs = map (`signGYTxBody` [uSKey]) bodies
  tids ← mapM (gySubmitTx providers) txs
  mapM_ (gyLogInfo providers "MM" . ("Submitted Tx: " ++) . show) tids
 where
  logWarn = gyLogWarning providers "Market Maker"

  hanldeSignAndSubmit ∷ SomeException → IO ()
  hanldeSignAndSubmit = logWarn . show

  getBodies = NE.toList . runIdentity . sequence

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

    let placeSkeletons = placeOrders netId mbUser (uaPlaces newActions) di
        cancelSkeletons = cancelOrders (uaCancels newActions) di
        allSkeletons = liftM2 (++) placeSkeletons cancelSkeletons

    signAndSubmit mbUser providers netId allSkeletons

    gyLogInfo providers "MM" "---------- Done for the block! -----------"
    threadDelay mbDelay
