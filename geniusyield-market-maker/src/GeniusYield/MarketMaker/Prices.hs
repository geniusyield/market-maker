module GeniusYield.MarketMaker.Prices where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.List (find, foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Deriving.Aeson
import GeniusYield.GYConfig
import GeniusYield.MarketMaker.Orphans ()
import GeniusYield.MarketMaker.Utils
import GeniusYield.OrderBot.DataSource.Providers (Connection)
import GeniusYield.OrderBot.OrderBook.AnnSet (
  MultiAssetOrderBook,
  OrderBook (..),
  Orders (..),
  maOrderBookToList,
  populateOrderBook,
  volumeGTPrice,
  volumeLTPrice,
 )
import GeniusYield.OrderBot.Types (
  OrderAssetPair (..),
  OrderType (..),
  Price (..),
  Volume (..),
 )
import GeniusYield.Providers.Maestro
import GeniusYield.Types
import Maestro.Client.V1
import Maestro.Types.V1

data SimTokenPair = SimTokenPair
  { currencySt ∷ SimToken,
    commoditySt ∷ SimToken
  }
  deriving stock (Eq, Ord, Show)

mkSimTokenPair ∷ SimToken → SimToken → SimTokenPair
mkSimTokenPair currSt commSt =
  SimTokenPair
    { currencySt = currSt,
      commoditySt = commSt
    }

toOAPair ∷ SimTokenPair → OrderAssetPair
toOAPair SimTokenPair {currencySt, commoditySt} =
  OAssetPair
    { currencyAsset = stAc currencySt,
      commodityAsset = stAc commoditySt
    }

data SimToken = SimToken
  { stAc ∷ GYAssetClass,
    stPrecision ∷ Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

lovelaceSt ∷ SimToken
lovelaceSt = SimToken {stAc = GYLovelace, stPrecision = 6}

data MaestroPairOverride = MaestroPairOverride
  { mpoCommodityToken ∷ !GYAssetClass,
    mpoPair ∷ !String,
    mpoCommodityIsFirst ∷ !Bool
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] MaestroPairOverride

data PriceConfig = PriceConfig
  { pcApiKey ∷ !(Confidential Text),
    pcNetworkId ∷ !GYNetworkId,
    pcDex ∷ !Dex,
    pcOverride ∷ !(Maybe MaestroPairOverride)
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] PriceConfig

data MaestroPP = MaestroPP
  { mppEnv ∷ !(MaestroEnv 'V1),
    mppDex ∷ !Dex,
    mppOverride ∷ !(Maybe MaestroPairOverride)
  }

data PricesProviders = PP
  { maestroPP ∷ !MaestroPP,
    orderBookPP ∷ !(Connection, DEXInfo)
  }

buildPP
  ∷ Connection
  → DEXInfo
  → PriceConfig
  → IO PricesProviders
buildPP c dex PriceConfig {..} =
  PP
    <$> ppMaestro
    <*> return (c, dex)
 where
  ppMaestro ∷ IO MaestroPP
  ppMaestro = do
    env ← networkIdToMaestroEnv (coerce pcApiKey) pcNetworkId
    return
      MaestroPP
        { mppEnv = env,
          mppDex = pcDex,
          mppOverride = pcOverride
        }

{-
    It contains: * Total sell volume in commodity asset
                 * Total buy volume in commodity asset

-}
data OBMarketTokenInfo = OBMarketTokenInfo
  { mtSellVol ∷ !Natural,
    mtBuyVol ∷ !Natural
  }
  deriving stock (Show)

type OBMarketInfo = M.Map SimTokenPair OBMarketTokenInfo

type MaestroMarketInfo = M.Map SimTokenPair Price

mkOBMarketTokenInfo
  ∷ Price
  → Rational
  → Orders 'SellOrder
  → Orders 'BuyOrder
  → OBMarketTokenInfo
mkOBMarketTokenInfo (Price marketPrice) spread sellOrders buyOrders =
  OBMarketTokenInfo
    { mtSellVol = volumeMax sumVolSell,
      mtBuyVol = floor $ toRational (volumeMax sumVolBuy) * marketPrice
    }
 where
  sumVolSell ∷ Volume
  sumVolSell = volumeLTPrice (Price (marketPrice + (marketPrice * spread))) sellOrders

  sumVolBuy ∷ Volume
  sumVolBuy = volumeGTPrice (Price (marketPrice - (marketPrice * spread))) buyOrders

getOrderBookPrices
  ∷ PricesProviders
  → [SimTokenPair]
  → MaestroMarketInfo
  → Rational
  → IO (OBMarketInfo, MultiAssetOrderBook)
getOrderBookPrices PP {orderBookPP = (c, dex)} stps mp priceCheckSpread = do
  maOrderBook ← populateOrderBook c dex (dexPORefs dex) (map toOAPair stps)
  return (M.fromList $ map buildPrice $ maOrderBookToList maOrderBook, maOrderBook)
 where
  buildPrice ∷ (OrderAssetPair, OrderBook) → (SimTokenPair, OBMarketTokenInfo)
  buildPrice (oap, ob) =
    let stPair = toSTPair oap
        price = M.lookup stPair mp & fromJust
        sndElement = uncurry (mkOBMarketTokenInfo price priceCheckSpread) . (sellOrders &&& buyOrders) $ ob
     in (stPair, sndElement)

  toSTPair ∷ OrderAssetPair → SimTokenPair
  toSTPair OAssetPair {currencyAsset, commodityAsset} =
    find (\SimTokenPair {..} → stAc currencySt == currencyAsset && stAc commoditySt == commodityAsset) stps
      & fromJust

getMaestroPrices
  ∷ PricesProviders
  → [SimTokenPair]
  → IO MaestroMarketInfo
getMaestroPrices PP {maestroPP = MaestroPP {..}} stps = do
  allDexPairs ← dexPairResponsePairs <$> pairsFromDex mppEnv mppDex

  let extendedInfo = mapMaybe isRelevantPairInfo allDexPairs
      stpInfos = foldl' (\m (stp, dpi, commodityIsA) → M.insert stp (dpi, commodityIsA) m) M.empty extendedInfo

  foldM
    ( \m stp →
        case findMaestroPair stp stpInfos of
          Nothing → do
            putStrLn $ "Could not find maestro pair for stp: " ++ show stp
            return m
          Just (name, commodityIsA) → do
            let pair = TaggedText $ pack name
            ohlInfo ← pricesFromDex mppEnv mppDex pair (Just Res5m) (Just Descending)

            let info = head ohlInfo
                curPrecision = stPrecision $ currencySt stp
                comPrecision = stPrecision $ commoditySt stp
                precisionDiff = 10 ** fromIntegral (curPrecision - comPrecision)

                price =
                  if commodityIsA
                    then ohlcCandleInfoCoinBClose info
                    else ohlcCandleInfoCoinAClose info

                adjustedPrice = price * precisionDiff

            return $ M.insert stp (Price $ toRational adjustedPrice) m
    )
    M.empty
    stps
 where
  isRelevantPairInfo ∷ DexPairInfo → Maybe (SimTokenPair, DexPairInfo, Bool)
  isRelevantPairInfo dpi@DexPairInfo {..} =
    (,dpi,False)
      <$> findMatchingSTP
        (dexPairInfoCoinAAssetName, dexPairInfoCoinAPolicy)
        (dexPairInfoCoinBAssetName, dexPairInfoCoinBPolicy)
      <|> (,dpi,True)
      <$> findMatchingSTP
        (dexPairInfoCoinBAssetName, dexPairInfoCoinBPolicy)
        (dexPairInfoCoinAAssetName, dexPairInfoCoinAPolicy)

  findMatchingSTP ∷ (TokenName, PolicyId) → (TokenName, PolicyId) → Maybe SimTokenPair
  findMatchingSTP tokenA tokenB = fromRight Nothing $ do
    assetClassA ← assetClassFromMaestro tokenA
    assetClassB ← assetClassFromMaestro tokenB
    Right $ find (\stp → assetClassA == stAc (currencySt stp) && assetClassB == stAc (commoditySt stp)) stps

  findMaestroPair ∷ SimTokenPair → M.Map SimTokenPair (DexPairInfo, Bool) → Maybe (String, Bool)
  findMaestroPair stp stpInfo =
    (mpoPair &&& mpoCommodityIsFirst)
      <$> find ((==) (stAc (commoditySt stp)) . mpoCommodityToken) mppOverride
      <|> first (T.unpack . dexPairInfoPair)
      <$> M.lookup stp stpInfo
