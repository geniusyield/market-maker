module GeniusYield.MarketMaker.Prices where

import Control.Applicative ((<|>))
import Control.Arrow (Arrow (first), (&&&))
import Control.Exception (Exception, throwIO, try)
import Control.Monad ((<=<))
import Data.Coerce (coerce)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
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
import GeniusYield.Providers.Common (silenceHeadersClientError)
import GeniusYield.Providers.Maestro
import GeniusYield.Types
import Maestro.Client.V1
import Maestro.Types.V1

data MaestroPriceException
  = MaestroPairNotFound
  | MaestroApiError !Text !MaestroError
  deriving stock (Show)
  deriving anyclass (Exception)

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
  { mpoPair ∷ !String,
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

type MaestroMarketInfo = M.Map SimTokenPair Price

getOrderBookPrices
  ∷ PricesProviders
  → [SimTokenPair]
  → Price
  → Rational
  → IO (OBMarketInfo, MultiAssetOrderBook)
getOrderBookPrices PP {orderBookPP = (c, dex)} stps price priceCheckSpread = do
  maOrderBook ← populateOrderBook c dex (dexPORefs dex) (map toOAPair stps)
  return (M.fromList $ map buildPrice $ maOrderBookToList maOrderBook, maOrderBook)
 where
  buildPrice ∷ (OrderAssetPair, OrderBook) → (SimTokenPair, OBMarketTokenInfo)
  buildPrice (oap, ob) =
    let stPair = toSTPair oap
        sndElement = uncurry (mkOBMarketTokenInfo price priceCheckSpread) . (sellOrders &&& buyOrders) $ ob
     in (stPair, sndElement)

  toSTPair ∷ OrderAssetPair → SimTokenPair
  toSTPair OAssetPair {currencyAsset, commodityAsset} =
    find (\SimTokenPair {..} → stAc currencySt == currencyAsset && stAc commoditySt == commodityAsset) stps
      & fromJust

-- | Remove headers (if `MaestroError` contains `ClientError`).
silenceHeadersMaestroClientError ∷ MaestroError → MaestroError
silenceHeadersMaestroClientError (ServantClientError e) = ServantClientError $ silenceHeadersClientError e
silenceHeadersMaestroClientError other = other

throwMspvApiError ∷ Text → MaestroError → IO a
throwMspvApiError locationInfo =
  throwIO . MaestroApiError locationInfo . silenceHeadersMaestroClientError

-- | Utility function to handle Maestro errors, which also removes header (if present) so as to conceal API key.
handleMaestroError ∷ Text → Either MaestroError a → IO a
handleMaestroError locationInfo = either (throwMspvApiError locationInfo) pure

getMaestroPrice
  ∷ PricesProviders
  → SimTokenPair
  → IO Price
getMaestroPrice PP {maestroPP = MaestroPP {..}} stp = do
  (pairName, commodityIsA) ← case mppOverride of
    -- We have to override with given details.
    Just (MaestroPairOverride {..}) → do
      pure (pack mpoPair, mpoCommodityIsFirst)
    -- We are given commodity token and need to find pair name.
    Nothing → do
      allDexPairs ← dexPairResponsePairs <$> (handleMaestroError "getMaestroPrice - fetching dex pairs" <=< try $ pairsFromDex mppEnv mppDex)

      let go [] = throwIO MaestroPairNotFound
          go (dpi : dpis) = maybe (go dpis) pure $ isRelevantPairInfo dpi
      first dexPairInfoPair <$> go allDexPairs

  let pair = TaggedText pairName

  ohlInfo ← handleMaestroError "getMaestroPrice - fetching price from pair" <=< try $ pricesFromDex mppEnv mppDex pair (Just Res5m) (Just Descending)

  let info = head ohlInfo
      curPrecision = stPrecision $ currencySt stp
      comPrecision = stPrecision $ commoditySt stp
      precisionDiff = 10 ** fromIntegral (curPrecision - comPrecision)

      price =
        if commodityIsA
          then ohlcCandleInfoCoinBClose info
          else ohlcCandleInfoCoinAClose info

      adjustedPrice = price * precisionDiff

  return $ Price (toRational adjustedPrice)
 where
  isRelevantPairInfo ∷ DexPairInfo → Maybe (DexPairInfo, Bool)
  isRelevantPairInfo dpi@DexPairInfo {..} =
    ( (dpi, False)
        <$ findMatchingSTP
          (dexPairInfoCoinAAssetName, dexPairInfoCoinAPolicy)
          (dexPairInfoCoinBAssetName, dexPairInfoCoinBPolicy)
    )
      <|> ( (dpi, True)
              <$ findMatchingSTP
                (dexPairInfoCoinBAssetName, dexPairInfoCoinBPolicy)
                (dexPairInfoCoinAAssetName, dexPairInfoCoinAPolicy)
          )

  findMatchingSTP ∷ (TokenName, PolicyId) → (TokenName, PolicyId) → Maybe SimTokenPair
  findMatchingSTP tokenA tokenB = fromRight Nothing $ do
    assetClassA ← assetClassFromMaestro tokenA
    assetClassB ← assetClassFromMaestro tokenB
    Right $ if assetClassA == stAc (currencySt stp) && assetClassB == stAc (commoditySt stp) then Just stp else Nothing
