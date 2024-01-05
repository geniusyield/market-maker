module GeniusYield.MarketMaker.Strategies where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Foldable
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Ratio (
  denominator,
  numerator,
  (%),
 )
import Data.Semigroup (Semigroup (stimes))
import Deriving.Aeson
import GHC.Natural (naturalFromInteger)
import GeniusYield.AnnSet.Internal (
  orderInfo,
  toAscList,
 )
import GeniusYield.Api.Dex.PartialOrder (
  PartialOrderInfo (..),
  poiGetContainedFeeValue,
 )
import GeniusYield.Imports (printf)
import GeniusYield.MarketMaker.Constants (logNS, makerFeeRatio)
import GeniusYield.MarketMaker.Prices
import GeniusYield.MarketMaker.User (User (..))
import GeniusYield.MarketMaker.Utils
import GeniusYield.OrderBot.DataSource.Providers (Connection (..))
import GeniusYield.OrderBot.OrderBook.AnnSet (
  MultiAssetOrderBook,
  OrderBook (..),
  Orders (unOrders),
  withEachAsset,
 )
import GeniusYield.OrderBot.Types
import GeniusYield.TxBuilder (
  GYTxQueryMonad (utxosAtAddress),
  runGYTxQueryMonadNode,
 )
import GeniusYield.Types

data UserActions = UserActions
  { uaPlaces ∷ [PlaceOrderAction],
    uaCancels ∷ [CancelOrderAction]
  }
  deriving stock (Show)

instance Semigroup UserActions where
  (<>) ua1 ua2 =
    UserActions
      { uaPlaces = uaPlaces ua1 <> uaPlaces ua2,
        uaCancels = uaCancels ua1 <> uaCancels ua2
      }

instance Monoid UserActions where
  mempty =
    UserActions
      { uaPlaces = [],
        uaCancels = []
      }

uaFromOnlyPlaces ∷ [PlaceOrderAction] → UserActions
uaFromOnlyPlaces poas = mempty {uaPlaces = poas}

uaFromOnlyCancels ∷ [CancelOrderAction] → UserActions
uaFromOnlyCancels coas = mempty {uaCancels = coas}

data PlaceOrderAction = PlaceOrderAction
  { poaOfferedAsset ∷ !GYAssetClass,
    poaOfferedAmount ∷ !Natural,
    poaAskedAsset ∷ !GYAssetClass,
    poaPrice ∷ !GYRational
  }
  deriving stock (Show)

newtype CancelOrderAction = CancelOrderAction {coaPoi ∷ PartialOrderInfo}
  deriving stock (Show)

type Strategy = PricesProviders → User → MMToken → IO UserActions

data StrategyConfig = StrategyConfig
  { scSpread ∷ !Rational,
    scPriceCheckProduct ∷ !Integer,
    scCancelThresholdProduct ∷ !Integer,
    scTokenVolume ∷ !TokenVol
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] StrategyConfig

data TokenVol = TokenVol
  { tvSellMinVol ∷ !Integer,
    tvBuyMinVol ∷ !Integer,
    tvSellBudget ∷ !Integer,
    tvBuyBudget ∷ !Integer,
    tvSellVolThreshold ∷ !Integer,
    tvBuyVolThreshold ∷ !Integer
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] TokenVol

-- | Uses a `MultiAssetOrderBook` to call `filterOwnOrders`.
getOwnOrders
  ∷ [MMTokenPair]
  → [User]
  → MultiAssetOrderBook
  → M.Map User [(MMTokenPair, PartialOrderInfo)]
getOwnOrders stps users maob =
  let sOrders = withEachAsset (\_ ob → toAscList $ unOrders $ sellOrders ob) maob
      bOrders = withEachAsset (\_ ob → toAscList $ unOrders $ buyOrders ob) maob
   in filterOwnOrders stps users (map (poi . orderInfo) sOrders ++ map (poi . orderInfo) bOrders)

{- | Given a list of relevant `MMTokenPair`'s, a list of users and a list of
      @PartialOrderInfo@. Returns a @Map@ between the users and the @PartialOrderInfo@'s
      that belong to that user and trade in one of the relevant pairs.
-}
filterOwnOrders
  ∷ [MMTokenPair]
  → [User]
  → [PartialOrderInfo]
  → M.Map User [(MMTokenPair, PartialOrderInfo)]
filterOwnOrders stps users allOrders =
  let usersPkh = map (pkhFromSkey . uSKey) users
      ourPOIs = filter (flip elem usersPkh . poiOwnerKey) allOrders
      relevantTokensPOIs = mapMaybe (filterTokenPair stps) ourPOIs
      finalMap = foldl' (\acc (stp, poi) → M.unionWith (++) acc (M.singleton (lookupUser poi) [(stp, poi)])) mempty relevantTokensPOIs
   in finalMap
 where
  filterTokenPair ∷ [MMTokenPair] → PartialOrderInfo → Maybe (MMTokenPair, PartialOrderInfo)
  filterTokenPair sTokenPairs poi@PartialOrderInfo {poiOfferedAsset, poiAskedAsset} =
    findStp assetPair1 <|> findStp assetPair2 <&> (,poi)
   where
    assetPair1 = mkOrderAssetPair poiOfferedAsset poiAskedAsset
    assetPair2 = mkOrderAssetPair poiAskedAsset poiOfferedAsset

    findStp ∷ OrderAssetPair → Maybe MMTokenPair
    findStp ap = find (\stp → ap == toOAPair stp) sTokenPairs

  lookupUser ∷ PartialOrderInfo → User
  lookupUser PartialOrderInfo {poiOwnerKey} =
    fromJust
      $ find ((==) poiOwnerKey . pkhFromSkey . uSKey) users

fixedSpreadVsMarketPriceStrategy ∷ StrategyConfig → Strategy
fixedSpreadVsMarketPriceStrategy
  StrategyConfig
    { scSpread,
      scPriceCheckProduct,
      scCancelThresholdProduct,
      scTokenVolume
    }
  pp
  user
  sToken = do
    let (Connection nid providers, _) = orderBookPP pp
        sTokenPair = mkMMTokenPair lovelaceSt sToken
        userAddr = (addrFromSkey nid . uSKey) user
        cancelThreshold = fromInteger scCancelThresholdProduct * scSpread
        priceCheckThreshold = fromInteger scPriceCheckProduct * scSpread

    mp ← getMaestroPrice pp sTokenPair

    logInfo providers $ logMaestroMarketInfo mp

    (bp, maob) ← getOrderBookPrices pp [sTokenPair] mp priceCheckThreshold

    ownUtxos ← runGYTxQueryMonadNode nid providers $ utxosAtAddress userAddr Nothing -- Assumption: User addresses does not include order validator's address.
    let ownOrdersPerUser = getOwnOrders [sTokenPair] [user] maob
        allOwnOrders = M.foldr (++) [] ownOrdersPerUser
        equityOnOrders = sum $ map (getEquityFromOrder mp) allOwnOrders
        totalValueOnUtxos = foldlUTxOs' (\acc utxo → acc <> utxoValue utxo) mempty ownUtxos
        equityOnUtxos = foldl' (\acc (_, n) → acc + n) 0 $ valueToList $ valueMap (getEquityFromValue mp) totalValueOnUtxos

        cancelOrderActions =
          map (\(_, poi) → CancelOrderAction poi)
            $ ordersToBeRemoved mp cancelThreshold allOwnOrders

        relevantSTP = mkMMTokenPair lovelaceSt sToken
        mtInfo = M.lookup relevantSTP bp

        lockedLovelaces = getOrdersLockedValue relevantSTP lovelaceSt allOwnOrders
        lockedTokens = getOrdersLockedValue relevantSTP sToken allOwnOrders

    placeOrderActions ← do
      let TokenVol
            { tvSellVolThreshold,
              tvBuyVolThreshold,
              tvSellMinVol,
              tvBuyMinVol,
              tvSellBudget,
              tvBuyBudget
            } = scTokenVolume

          (sellVol, buyVol) = case mtInfo of
            Nothing → (0, 0)
            Just OBMarketTokenInfo {mtSellVol, mtBuyVol} → (mtSellVol, mtBuyVol)

          availableBuyBudget = max 0 (tvBuyBudget - fromIntegral lockedLovelaces)
          availableSellBudget = max 0 (tvSellBudget - fromIntegral lockedTokens)
          numNewBuyOrders = availableBuyBudget `quot` tvBuyMinVol
          numNewSellOrders = availableSellBudget `quot` tvSellMinVol
          adaOverhead = valueFromLovelace 5_000_000
          subtractTillZero ∷ GYValue → GYValue → Natural → Natural
          subtractTillZero val sub acc = if val `valueGreaterOrEqual` sub then subtractTillZero (val `valueMinus` sub) sub (acc + 1) else acc

      (newBuyOrders, totalValueOnUtxosAfterBuyOrds) ←
        if tvBuyVolThreshold <= fromIntegral buyVol || numNewBuyOrders == 0
          then pure ([], totalValueOnUtxos)
          else do
            let tokensToOfferPerOrder = availableBuyBudget `quot` numNewBuyOrders
                neededAtleastPerOrder = valueFromLovelace (ceiling $ toRational tokensToOfferPerOrder * (1 + makerFeeRatio)) <> adaOverhead
                neededAtleast = stimes numNewBuyOrders neededAtleastPerOrder
                valueSufficient = totalValueOnUtxos `valueGreaterOrEqual` neededAtleast
                actualNumNewBuyOrders = if valueSufficient then numNewBuyOrders else fromIntegral $ subtractTillZero totalValueOnUtxos neededAtleastPerOrder 0
                totalValueOnUtxosAfterBuyOrds = totalValueOnUtxos `valueMinus` stimes actualNumNewBuyOrders neededAtleastPerOrder

            unless valueSufficient $ logWarn providers $ printf "Bot has to place %d buy order(s), but lack funds, total balance (excluding collateral) should be at least: %s but available funds are: %s. Only placing %d buy order(s)." numNewBuyOrders (show neededAtleast) (show totalValueOnUtxos) actualNumNewBuyOrders

            pure
              ( buildNewUserOrders
                  scSpread
                  (sToken, lovelaceSt)
                  mp
                  (fromIntegral tokensToOfferPerOrder)
                  (fromIntegral actualNumNewBuyOrders)
                  True,
                totalValueOnUtxosAfterBuyOrds
              )

      newSellOrders ←
        if tvSellVolThreshold <= fromIntegral sellVol || numNewSellOrders == 0
          then pure []
          else do
            let tokensToOfferPerOrder = availableSellBudget `quot` numNewSellOrders
                neededAtleastPerOrder = valueSingleton (mmtAc sToken) (ceiling $ toRational tokensToOfferPerOrder * (1 + makerFeeRatio)) <> adaOverhead
                neededAtleast = stimes numNewSellOrders neededAtleastPerOrder
                valueSufficient = totalValueOnUtxosAfterBuyOrds `valueGreaterOrEqual` neededAtleast
                actualNumNewSellOrders = if valueSufficient then numNewSellOrders else fromIntegral $ subtractTillZero totalValueOnUtxosAfterBuyOrds neededAtleastPerOrder 0

            unless valueSufficient $ logWarn providers $ printf "Bot has to place %d sell order(s), but lack funds, total balance (excluding collateral & value reserved for buy order(s)) should be at least: %s but available funds are: %s. Only placing %d sell order(s)." numNewSellOrders (show neededAtleast) (show totalValueOnUtxosAfterBuyOrds) actualNumNewSellOrders
            pure
              $ buildNewUserOrders
                scSpread
                (lovelaceSt, sToken)
                mp
                (fromIntegral $ availableSellBudget `quot` numNewSellOrders)
                (fromIntegral actualNumNewSellOrders)
                False
      pure $ newBuyOrders <> newSellOrders

    let placeUserActions = uaFromOnlyPlaces placeOrderActions
        cancelUserActions = uaFromOnlyCancels cancelOrderActions

    let adaInLovelaces = 1_000_000
    -- This 3 logs can be used for metrics, don't update without making sure it's okay to do so
    logInfo providers $ "Equity on Orders: " ++ show (equityOnOrders `div` adaInLovelaces) ++ " ADAs"
    logInfo providers $ "Equity on Wallet: " ++ show (equityOnUtxos `div` fromIntegral adaInLovelaces) ++ " ADAs"
    logInfo providers $ "Total Equity: " ++ show ((equityOnOrders + fromInteger equityOnUtxos) `div` adaInLovelaces) ++ " ADAs"

    logDebug providers $ "Place Actions: " ++ show placeOrderActions
    logDebug providers $ "Cancel Actions: " ++ show cancelOrderActions

    logInfo providers $ unlines ("Orders being placed:" : map logPlaceAction placeOrderActions)
    logInfo providers $ unlines ("Orders being canceled:" : map (show . poiRef . coaPoi) cancelOrderActions)
    logInfo providers "----------------- FINISHED STRATEGY ---------------"

    return $ placeUserActions <> cancelUserActions
   where
    buildNewUserOrders
      ∷ Rational
      → (MMToken, MMToken)
      → Price
      → Natural
      → Natural
      → Bool
      → [PlaceOrderAction]
    buildNewUserOrders delta' (ask, off) p tokenQ nOrders toInverse =
      let p' = getPrice p
          poi n =
            let newMPrice = (1 + (1 + 0.5 * toRational n) * (if toInverse then -1 else 1) * delta') * p'
             in PlaceOrderAction
                  { poaOfferedAsset = mmtAc off,
                    poaOfferedAmount = naturalFromInteger $ fromIntegral tokenQ,
                    poaAskedAsset = mmtAc ask,
                    poaPrice = rationalFromGHC $ if toInverse then denominator newMPrice % numerator newMPrice else newMPrice
                  }
       in if nOrders == 0 then [] else map poi [0 .. (nOrders - 1)] -- `nOrders` has type `Natural` thus subtracting from zero can give arithmetic exception.
    getEquityFromOrder ∷ Price → (MMTokenPair, PartialOrderInfo) → Natural
    getEquityFromOrder price (_stp, poi) =
      let ownFunds = getOrderOwnFunds poi
          priceOfNonAdaToken nonAdaAC = floor $ fromIntegral (valueAssetClass ownFunds nonAdaAC) * getPrice price
       in (valueAssetClass ownFunds GYLovelace & fromIntegral)
            + ( if poiOfferedAsset poi == GYLovelace
                  then priceOfNonAdaToken (poiAskedAsset poi)
                  else priceOfNonAdaToken (poiOfferedAsset poi)
              )
     where
      -- \| Note that at any moment, an order UTxO contains:-
      --                  * An NFT.
      --                  * Remaining offered tokens.
      --                  * Payment for tokens consumed.
      --                  * Initial deposit.
      --                  * Collected fees.
      --
      getOrderOwnFunds ∷ PartialOrderInfo → GYValue
      getOrderOwnFunds PartialOrderInfo {..} =
        let toSubtract = valueSingleton (GYToken poiNFTCS poiNFT) 1 <> poiGetContainedFeeValue poi
         in poiUTxOValue `valueMinus` toSubtract

    getEquityFromValue ∷ Price → GYAssetClass → Integer → Integer
    getEquityFromValue _ GYLovelace n = n
    getEquityFromValue (getPrice → price) _ac n =
      floor $ price * fromInteger n

    getOrdersLockedValue ∷ MMTokenPair → MMToken → [(MMTokenPair, PartialOrderInfo)] → Natural
    getOrdersLockedValue stp st orders =
      let relevantOfferedAc = mmtAc st
          relevantOrders = filter (\(oStp, oPoi) → oStp == stp && relevantOfferedAc == poiOfferedAsset oPoi) orders
       in sum $ map (poiOfferedAmount . snd) relevantOrders

    logInfo, logDebug, logWarn ∷ GYProviders → String → IO ()
    logInfo providers = gyLogInfo providers logNS
    logDebug providers = gyLogDebug providers logNS
    logWarn providers = gyLogWarning providers logNS

    logPlaceAction ∷ PlaceOrderAction → String
    logPlaceAction PlaceOrderAction {..} =
      let price = (fromRational (rationalToGHC poaPrice) ∷ Double)
          adjustedPrice = 1 / price
       in unwords
            [ "Selling",
              show poaOfferedAmount,
              prettyAc poaOfferedAsset,
              "for",
              show price,
              prettyAc poaAskedAsset,
              "each",
              "(inverted price:",
              show adjustedPrice,
              ")"
            ]
    logMaestroMarketInfo ∷ Price → String
    logMaestroMarketInfo price =
      unwords
        [ "Price for:",
          prettyAc $ mmtAc sToken,
          "is",
          show (fromRational (getPrice price) ∷ Double)
        ]

    prettyAc ∷ GYAssetClass → String
    prettyAc GYLovelace = "lovelaces"
    prettyAc (GYToken _ tn) = "indivisible of " ++ show tn

ordersToBeRemoved ∷ Price → Rational → [(MMTokenPair, PartialOrderInfo)] → [(MMTokenPair, PartialOrderInfo)]
ordersToBeRemoved price cancelLimitSpread = filter (orderIsToBeRemoved price cancelLimitSpread)

orderIsToBeRemoved ∷ Price → Rational → (MMTokenPair, PartialOrderInfo) → Bool
orderIsToBeRemoved mPrice cancelLimitSpread (stp, poi) =
  let marketPrice = getPrice mPrice
      oap = toOAPair stp
   in case mkOrderInfo oap poi of
        SomeOrderInfo OrderInfo {orderType = SBuyOrder, price} → getPrice price < marketPrice - (cancelLimitSpread * marketPrice)
        SomeOrderInfo OrderInfo {orderType = SSellOrder, price} → getPrice price > marketPrice + (cancelLimitSpread * marketPrice)
