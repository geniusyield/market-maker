module GeniusYield.MarketMaker.Strategies
  ( UserActions (..)
  , UserActionsController (..)
  , uaFromOnlyPlaces
  , uaFromOnlyCancels
  , PlaceOrderAction (..)
  , CancelOrderAction (..)
  , Strategy
  , PreemptiveCancelSpreadRatio
  , mkPreemptiveCancelSpreadRatio
  , unPreemptiveCancelSpreadRatio
  , StrategyConfig (..)
  , TokenVol (..)
  , getOwnOrders
  , fixedSpreadVsMarketPriceStrategy
  ) where

import           Control.Applicative                       ((<|>))
import           Control.Monad                             (unless)
import           Data.Aeson                                (FromJSON (..))
import           Data.Foldable
import           Data.Function                             ((&))
import           Data.Functor                              ((<&>))
import           Data.List                                 (nub, (\\))
import qualified Data.Map.Strict                           as M
import           Data.Maybe                                (fromJust, fromMaybe,
                                                            mapMaybe)
import           Data.Ratio                                (denominator,
                                                            numerator, (%))
import           Data.Semigroup                            (Semigroup (stimes),
                                                            mtimesDefault)
import qualified Data.Set                                  as Set
import           Deriving.Aeson
import           GeniusYield.AnnSet.Internal               (orderInfo,
                                                            toAscList)
import           GeniusYield.Api.Dex.PartialOrder          (PartialOrderInfo (..),
                                                            poiGetContainedFeeValue)
import           GeniusYield.Imports                       (coerce, printf)
import           GeniusYield.MarketMaker.Constants         (logNS,
                                                            makerFeeRatio)
import           GeniusYield.MarketMaker.Equity
import           GeniusYield.MarketMaker.Prices
import           GeniusYield.MarketMaker.Spread            (Spread (..))
import           GeniusYield.MarketMaker.User              (User (..))
import           GeniusYield.MarketMaker.Utils
import           GeniusYield.OrderBot.DataSource.Providers (Connection (..))
import           GeniusYield.OrderBot.OrderBook.AnnSet     (MultiAssetOrderBook,
                                                            OrderBook (..),
                                                            Orders (unOrders),
                                                            withEachAsset)
import           GeniusYield.OrderBot.Types
import           GeniusYield.TxBuilder                     (GYTxQueryMonad (utxosAtAddress),
                                                            runGYTxQueryMonadNode)
import           GeniusYield.Types
import           GHC.Natural                               (naturalFromInteger)

-- $setup
-- >>> import qualified Data.Aeson as Aeson

data UserActions = UserActions
  { uaPlaces  :: [PlaceOrderAction],
    uaCancels :: [CancelOrderAction]
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

data UserActionsController = UACNormal | UACSpooked1 | UACSpooked2 String

uaFromOnlyPlaces :: [PlaceOrderAction]-> UserActions
uaFromOnlyPlaces poas = mempty {uaPlaces = poas}

uaFromOnlyCancels :: [CancelOrderAction] -> UserActions
uaFromOnlyCancels coas = mempty {uaCancels = coas}

data PlaceOrderAction = PlaceOrderAction
  { poaOfferedAsset  :: !GYAssetClass,
    poaOfferedAmount :: !Natural,
    poaAskedAsset    :: !GYAssetClass,
    poaPrice         :: !GYRational
  }
  deriving stock (Show)

newtype CancelOrderAction = CancelOrderAction {coaPoi :: PartialOrderInfo}
  deriving stock (Show)

type Strategy = PricesProviders -> User -> MMToken -> IO (UserActions, UserActionsController)

newtype PreemptiveCancelSpreadRatio = PreemptiveCancelSpreadRatio { unPreemptiveCancelSpreadRatio :: Rational }
  deriving stock Show
  deriving newtype (ToJSON, Num, Eq, Ord)

-- >>> Aeson.eitherDecode @PreemptiveCancelSpreadRatio "{\"numerator\": 101, \"denominator\": 100}"
-- Left "Error in $: sc_preemptive_cancel_spread_ratio parameter must be b/w 0 and 1"
--
-- >>> Aeson.eitherDecode @PreemptiveCancelSpreadRatio "{\"numerator\": 100, \"denominator\": 100}"
-- Right (PreemptiveCancelSpreadRatio {unPreemptiveCancelSpreadRatio = 1 % 1})
--
-- >>> Aeson.eitherDecode @PreemptiveCancelSpreadRatio "{\"numerator\": -1, \"denominator\": 100}"
-- Left "Error in $: sc_preemptive_cancel_spread_ratio parameter must be b/w 0 and 1"
--
-- >>> Aeson.eitherDecode @PreemptiveCancelSpreadRatio "{\"numerator\": 0, \"denominator\": 100}"
-- Right (PreemptiveCancelSpreadRatio {unPreemptiveCancelSpreadRatio = 0 % 1})
instance FromJSON PreemptiveCancelSpreadRatio where
  parseJSON v = do
    v' :: Rational <- parseJSON v
    case mkPreemptiveCancelSpreadRatio v' of
      Left s  -> fail s
      Right a -> pure a

mkPreemptiveCancelSpreadRatio :: Rational -> Either String PreemptiveCancelSpreadRatio
mkPreemptiveCancelSpreadRatio n =
  if n >= 0 && n <= 1 then Right $ coerce n
  else Left "sc_preemptive_cancel_spread_ratio parameter must be b/w 0 and 1"

data StrategyConfig = StrategyConfig
  { scSpread                      :: !Spread,
    scPriceCheckProduct           :: !Integer,
    scCancelThresholdProduct      :: !Integer,
    scTokenVolume                 :: !TokenVol,
    scPreemptiveCancelSpreadRatio :: !(Maybe PreemptiveCancelSpreadRatio)
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] StrategyConfig

data TokenVol = TokenVol
  { tvSellMinVol       :: !Integer,
    tvBuyMinVol        :: !Integer,
    tvSellBudget       :: !Integer,
    tvBuyBudget        :: !Integer,
    tvSellVolThreshold :: !Integer,
    tvBuyVolThreshold  :: !Integer
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] TokenVol

-- | Uses a `MultiAssetOrderBook` to call `filterOwnOrders`.
getOwnOrders
  :: [MMTokenPair]
  -> [User]
  -> MultiAssetOrderBook
  -> M.Map User [(MMTokenPair, PartialOrderInfo)]
getOwnOrders mmts users maob =
  let sOrders = withEachAsset (\_ ob -> toAscList $ unOrders $ sellOrders ob) maob
      bOrders = withEachAsset (\_ ob -> toAscList $ unOrders $ buyOrders ob) maob
   in filterOwnOrders mmts users (map (poi . orderInfo) sOrders ++ map (poi . orderInfo) bOrders)

{- | Given a list of relevant `MMTokenPair`'s, a list of users and a list of
      @PartialOrderInfo@. Returns a @Map@ between the users and the @PartialOrderInfo@'s
      that belong to that user and trade in one of the relevant pairs.
-}
filterOwnOrders
  :: [MMTokenPair]
  -> [User]
  -> [PartialOrderInfo]
  -> M.Map User [(MMTokenPair, PartialOrderInfo)]
filterOwnOrders mmts users allOrders =
  let usersPkh = toPubKeyHash . pkhUser <$> users
      ourPOIs = filter (flip elem usersPkh . poiOwnerKey) allOrders
      relevantTokensPOIs = mapMaybe (filterTokenPair mmts) ourPOIs
      finalMap = foldl' (\acc (mmtp, poi) -> M.unionWith (++) acc (M.singleton (lookupUser poi) [(mmtp, poi)])) mempty relevantTokensPOIs
   in finalMap
 where
  filterTokenPair :: [MMTokenPair] -> PartialOrderInfo -> Maybe (MMTokenPair, PartialOrderInfo)
  filterTokenPair mmTokenPairs poi@PartialOrderInfo {poiOfferedAsset, poiAskedAsset} =
    findStp assetPair1 <|> findStp assetPair2 <&> (,poi)
   where
    assetPair1 = mkOrderAssetPair poiOfferedAsset poiAskedAsset
    assetPair2 = mkOrderAssetPair poiAskedAsset poiOfferedAsset

    findStp :: OrderAssetPair -> Maybe MMTokenPair
    findStp ap = find (\mmtp -> ap == toOAPair mmtp) mmTokenPairs

  lookupUser :: PartialOrderInfo -> User
  lookupUser PartialOrderInfo {poiOwnerKey} =
    fromJust
      $ find ((==) poiOwnerKey . toPubKeyHash . pkhUser) users

withPriceEstimate :: (Price -> IO UserActions) -> Strategy
withPriceEstimate k pp _ mmt = do
  let mmTokenPair = mkMMTokenPair mmtLovelace mmt
      (Connection _ providers, _) = orderBookPP pp

  pe <- priceEstimate pp mmTokenPair
  case pe of
    Left (PriceSourceFail p) -> do
      logWarn providers $ "One prices provider failed"  -- TODO: give details of failing provider
      actions <- k p
      return (actions, UACNormal)
    Left PriceMismatch1      -> return (mempty, UACSpooked1)
    Left PriceMismatch2      -> return (mempty, UACSpooked2 "outrageous price mismatch among Prices Providers")
    Left PriceUnavailable    -> return (mempty, UACSpooked2 "all Prices Providers unavailable")
    Right p                  -> k p >>= \actions -> pure (actions, UACNormal)
 where
  logWarn :: GYProviders -> String -> IO ()
  logWarn providers = gyLogWarning providers logNS

fixedSpreadVsMarketPriceStrategy :: StrategyConfig -> Strategy
fixedSpreadVsMarketPriceStrategy
  sc@StrategyConfig { .. }
  pp
  user
  mmToken = do
    let (Connection nid providers, _) = orderBookPP pp
        mmTokenPair = mkMMTokenPair mmtLovelace mmToken
        userAddr = addrUser nid user
        cancelThreshold = fromInteger scCancelThresholdProduct * scSpread
        priceCheckThreshold = fromInteger scPriceCheckProduct * scSpread

    logInfo providers $ "Strategy configuration: " <> show  sc

    flip4 withPriceEstimate pp user mmToken $ \mp -> do
      logInfo providers $ logMarketInfo mp
      
      (bp, maob) <- getOrderBookPrices pp [mmTokenPair] mp priceCheckThreshold

      ownUtxos <- runGYTxQueryMonadNode nid providers $ utxosAtAddress userAddr Nothing -- Assumption: User addresses does not include order validator's address.
      let ownOrdersPerUser = getOwnOrders [mmTokenPair] [user] maob
          allOwnOrders = M.foldr (++) [] ownOrdersPerUser
          equityInOrders = foldMap' getEquityFromOrder allOwnOrders
          equityInWallet = equityFromValue $ foldlUTxOs' (\acc utxo -> acc <> utxoValue utxo) mempty ownUtxos

          ordersToCancel =
            let mp' = getPrice mp
                scPreemptiveCancelSpreadRatio' = unPreemptiveCancelSpreadRatio $ fromMaybe 0 scPreemptiveCancelSpreadRatio
                priceCrosses (toOAPair -> oap, poi) =
                  case mkOrderInfo oap poi of
                    SomeOrderInfo OrderInfo { orderType = SSellOrder, price } -> getPrice price <= mp' * (1 + sellSideSpread scSpread * scPreemptiveCancelSpreadRatio')
                    SomeOrderInfo OrderInfo { orderType = SBuyOrder, price } -> getPrice price >= mp' * (1 - buySideSpread scSpread * scPreemptiveCancelSpreadRatio')
            in
              nub $
                   -- Cancel placed orders which are crossed by market price.
                   filter priceCrosses allOwnOrders
                <> -- And also cancel those which are "too away" from market price.
                   ordersToBeRemoved mp cancelThreshold allOwnOrders
          cancelOrderActions = map (CancelOrderAction . snd) ordersToCancel

          relevantMMTP = mkMMTokenPair mmtLovelace mmToken
          mtInfo = M.lookup relevantMMTP bp
     
          ownRemainingOrders = allOwnOrders \\ ordersToCancel
          lockedLovelaces = getOrdersLockedValue relevantMMTP mmtLovelace ownRemainingOrders
          lockedTokens = getOrdersLockedValue relevantMMTP mmToken ownRemainingOrders
          equityInWalletAfterCancelActions = equityInWallet <> foldMap' getEquityFromOrder ordersToCancel
          tokensSet = Set.fromList [mmtLovelace, mmToken]

      placeOrderActions <- do
        let TokenVol
              { tvSellVolThreshold,
                tvBuyVolThreshold,
                tvSellMinVol,
                tvBuyMinVol,
                tvSellBudget,
                tvBuyBudget
              } = scTokenVolume

            (sellVol, buyVol) = case mtInfo of
              Nothing -> (0, 0)
              Just OBMarketTokenInfo { mtSellVol, mtBuyVol } -> (mtSellVol, mtBuyVol)

            availableBuyBudget = max 0 (tvBuyBudget - fromIntegral lockedLovelaces)
            availableSellBudget = max 0 (tvSellBudget - fromIntegral lockedTokens)
            numNewBuyOrders = availableBuyBudget `quot` tvBuyMinVol
            numNewSellOrders = availableSellBudget `quot` tvSellMinVol
            adaOverhead = valueFromLovelace 5_000_000
            subtractTillZero :: GYValue -> GYValue -> Natural -> Natural
            subtractTillZero val sub acc = if val `valueGreaterOrEqual` sub then subtractTillZero (val `valueMinus` sub) sub (acc + 1) else acc
        -- TODO: To subtract from `buyVol` pertaining to order cancellations? Likewise for `sellVol`.
        -- TODO: Abstract out both buy order actions & sell order actions to a single function.
        (newBuyOrders, equityInWalletAfterBuyOrds) <-
          if tvBuyVolThreshold <= fromIntegral buyVol || numNewBuyOrders == 0
            then pure ([], equityInWalletAfterCancelActions)
            else do
              let tokensToOfferPerOrder = availableBuyBudget `quot` numNewBuyOrders
                  neededAtleastPerOrder = valueFromLovelace (ceiling $ toRational tokensToOfferPerOrder * (1 + makerFeeRatio)) <> adaOverhead
                  neededAtleast = stimes numNewBuyOrders neededAtleastPerOrder
                  valueInWalletAfterCancelActions = equityToValue equityInWalletAfterCancelActions
                  valueSufficient = valueInWalletAfterCancelActions `valueGreaterOrEqual` neededAtleast
                  actualNumNewBuyOrders = if valueSufficient then numNewBuyOrders else fromIntegral $ subtractTillZero valueInWalletAfterCancelActions neededAtleastPerOrder 0
                  equityInWalletAfterBuyOrds = equityFromValue $ valueInWalletAfterCancelActions `valueMinus` mtimesDefault actualNumNewBuyOrders neededAtleastPerOrder

              unless valueSufficient $ logWarn providers $ printf "Bot has to place %d buy order(s), but lack funds, total balance (excluding collateral) should be at least: %s\n but available funds are: %s.\n Only placing %d buy order(s)." numNewBuyOrders (showEquity (equityFromValue neededAtleast) tokensSet) (showEquity equityInWalletAfterCancelActions tokensSet) actualNumNewBuyOrders

              pure
                ( buildNewUserOrders
                    scSpread
                    (mmToken, mmtLovelace)
                    mp
                    (fromIntegral tokensToOfferPerOrder)
                    (fromIntegral actualNumNewBuyOrders)
                    True,
                  equityInWalletAfterBuyOrds
                )

        newSellOrders <-
          if tvSellVolThreshold <= fromIntegral sellVol || numNewSellOrders == 0
            then pure []
            else do
              let tokensToOfferPerOrder = availableSellBudget `quot` numNewSellOrders
                  neededAtleastPerOrder = valueSingleton (mmtAc mmToken) (ceiling $ toRational tokensToOfferPerOrder * (1 + makerFeeRatio)) <> adaOverhead
                  neededAtleast = stimes numNewSellOrders neededAtleastPerOrder
                  valueInWalletAfterBuyOrds = equityToValue equityInWalletAfterBuyOrds
                  valueSufficient = valueInWalletAfterBuyOrds `valueGreaterOrEqual` neededAtleast
                  actualNumNewSellOrders = if valueSufficient then numNewSellOrders else fromIntegral $ subtractTillZero valueInWalletAfterBuyOrds neededAtleastPerOrder 0

              unless valueSufficient $ logWarn providers $ printf "Bot has to place %d sell order(s), but lack funds, total balance (excluding collateral & value reserved for buy order(s)) should be at least: %s\n but available funds are: %s.\n Only placing %d sell order(s)." numNewSellOrders (showEquity (equityFromValue neededAtleast) tokensSet) (showEquity equityInWalletAfterBuyOrds tokensSet) actualNumNewSellOrders
              pure
                $ buildNewUserOrders
                  scSpread
                  (mmtLovelace, mmToken)
                  mp
                  (fromIntegral $ availableSellBudget `quot` numNewSellOrders)
                  (fromIntegral actualNumNewSellOrders)
                  False
        pure $ newBuyOrders <> newSellOrders

      let placeUserActions = uaFromOnlyPlaces placeOrderActions
          cancelUserActions = uaFromOnlyCancels cancelOrderActions
          totalEquity = equityInWallet <> equityInOrders

      -- This 3 logs can be used for metrics, don't update without making sure it's okay to do so
      logInfo providers $ "Equity in Orders: " ++ showEquity equityInOrders tokensSet
      logInfo providers $ "Equity in Wallet: " ++ showEquity equityInWallet tokensSet
      logInfo providers $ "Total Equity: " ++ showEquity totalEquity tokensSet
      logInfo providers $ "Total Equity normalized into ADA: " ++ showTokenAmount mmtLovelace (fromIntegral $ normalizeEquity totalEquity (M.fromList [(GYLovelace, Price 1), (mmtAc mmToken, mp)]))

      logDebug providers $ "Place Actions: " ++ show placeOrderActions
      logDebug providers $ "Cancel Actions: " ++ show cancelOrderActions

      logInfo providers $ unlines ("Orders being placed:" : map logPlaceAction placeOrderActions)
      logInfo providers $ unlines ("Orders being canceled:" : map (show . poiRef . coaPoi) cancelOrderActions)
      logInfo providers "----------------- FINISHED STRATEGY ---------------"

      return $ placeUserActions <> cancelUserActions
     where
      buildNewUserOrders
        :: Spread
        -> (MMToken, MMToken)
        -> Price
        -> Natural
        -> Natural
        -> Bool
        -> [PlaceOrderAction]
      buildNewUserOrders _delta@Spread {..} (ask, off) p tokenQ nOrders toInverse =
        let p' = getPrice p
            poi n =
              let newMPrice = (1 + (1 + 0.5 * toRational n) * (if toInverse then -1 * buySideSpread else sellSideSpread)) * p'
               in PlaceOrderAction
                    { poaOfferedAsset = mmtAc off,
                      poaOfferedAmount = naturalFromInteger $ fromIntegral tokenQ,
                      poaAskedAsset = mmtAc ask,
                      poaPrice = rationalFromGHC $ if toInverse then denominator newMPrice % numerator newMPrice else newMPrice
                    }
         in if nOrders == 0 then [] else map poi [0 .. (nOrders - 1)] -- `nOrders` has type `Natural` thus subtracting from zero can give arithmetic exception.
      getEquityFromOrder :: (MMTokenPair, PartialOrderInfo) -> Equity
      getEquityFromOrder (_mmtp, poi) = getOrderOwnFunds poi & equityFromValue
       where
        -- \| Note that at any moment, an order UTxO contains:-
        --                  * An NFT.
        --                  * Remaining offered tokens.
        --                  * Payment for tokens consumed.
        --                  * Initial deposit.
        --                  * Collected fees.
        --
        getOrderOwnFunds :: PartialOrderInfo -> GYValue
        getOrderOwnFunds PartialOrderInfo {..} =
          let toSubtract = valueSingleton (GYToken poiNFTCS poiNFT) 1 <> poiGetContainedFeeValue poi
           in poiUTxOValue `valueMinus` toSubtract

      getOrdersLockedValue :: MMTokenPair -> MMToken -> [(MMTokenPair, PartialOrderInfo)] -> Natural
      getOrdersLockedValue mmtp st orders =
        let relevantOfferedAc = mmtAc st
            relevantOrders = filter (\(oStp, oPoi) -> oStp == mmtp && relevantOfferedAc == poiOfferedAsset oPoi) orders
         in sum $ map (poiOfferedAmount . snd) relevantOrders

      logInfo, logDebug, logWarn :: GYProviders -> String -> IO ()
      logInfo providers = gyLogInfo providers logNS
      logDebug providers = gyLogDebug providers logNS
      logWarn providers = gyLogWarning providers logNS

      logPlaceAction :: PlaceOrderAction -> String
      logPlaceAction PlaceOrderAction {..} =
        let price = (fromRational (rationalToGHC poaPrice) :: Double)
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
      logMarketInfo :: Price -> String
      logMarketInfo price =
        unwords
          [ "Price for:",
            prettyAc $ mmtAc mmToken,
            "is",
            show (fromRational (getPrice price) :: Double),
            "lovelaces"
          ]

      prettyAc :: GYAssetClass -> String
      prettyAc GYLovelace     = "lovelaces"
      prettyAc (GYToken _ tn) = "indivisible of " ++ show tn

ordersToBeRemoved :: Price -> Spread -> [(MMTokenPair, PartialOrderInfo)] -> [(MMTokenPair, PartialOrderInfo)]
ordersToBeRemoved price cancelLimitSpread = filter (orderIsToBeRemoved price cancelLimitSpread)

orderIsToBeRemoved :: Price -> Spread -> (MMTokenPair, PartialOrderInfo) -> Bool
orderIsToBeRemoved mPrice _cancelLimitSpread@Spread {..} (mmtp, poi) =
  let marketPrice = getPrice mPrice
      oap = toOAPair mmtp
   in case mkOrderInfo oap poi of
        SomeOrderInfo OrderInfo {orderType = SBuyOrder, price} -> getPrice price < marketPrice - (buySideSpread * marketPrice)
        SomeOrderInfo OrderInfo {orderType = SSellOrder, price} -> getPrice price > marketPrice + (sellSideSpread * marketPrice)

flip4 :: (a -> b -> c -> d -> r) -> (b -> c -> d -> a -> r)
flip4 f = \b c d a -> f a b c d

