{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module GeniusYield.MarketMaker.Prices where

import           Control.Applicative                       ((<|>))
import           Control.Arrow                             (Arrow (first),
                                                            (&&&))
import           Control.Exception                         (Exception, throwIO,
                                                            try)
import           Control.Monad                             ((<=<))
import           Data.ByteString.Char8                     (unpack)
import           Data.Coerce                               (coerce)
import           Data.Either                               (fromRight)
import           Data.Fixed                                (Fixed (..),
                                                            showFixed)
import           Data.Function                             ((&))
import           Data.List                                 (find)
import qualified Data.Map.Strict                           as M
import           Data.Maybe                                (fromJust)
import           Data.Text                                 (Text, pack)
import           Deriving.Aeson
import           GeniusYield.Api.Dex.Constants             (DEXInfo (..))
import           GeniusYield.GYConfig
import           GeniusYield.Imports                       (Proxy)
import           GeniusYield.MarketMaker.Spread            (Spread (..))
import           GeniusYield.MarketMaker.Utils
import           GeniusYield.OrderBot.DataSource.Providers (Connection)
import           GeniusYield.OrderBot.OrderBook.AnnSet     (MultiAssetOrderBook,
                                                            OrderBook (..),
                                                            Orders (..),
                                                            maOrderBookToList,
                                                            populateOrderBook,
                                                            volumeGTPrice,
                                                            volumeLTPrice)
import           GeniusYield.OrderBot.Types                (OrderAssetPair (..),
                                                            OrderType (..),
                                                            Price (..),
                                                            Volume (..))
import           GeniusYield.Providers.Common              (silenceHeadersClientError)
import           GeniusYield.Providers.Maestro
import           GeniusYield.Types
import           GHC.TypeLits                              (type (^))
import           GHC.TypeNats                              (SomeNat (SomeNat),
                                                            someNatVal)
import           Maestro.Client.V1
import           Maestro.Types.V1

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import GeniusYield.Types
-- >>> let gensAC = "dda5fdb1002f7389b33e036b6afee82a8189becb6cba852e8b79b4fb.0014df1047454e53"
-- >>> let frenAC = "fc11a9ef431f81b837736be5f53e4da29b9469c983d07f321262ce61.4652454e"

data MaestroPriceException
  = MaestroPairNotFound
  | MaestroApiError !Text !MaestroError
  deriving stock (Show)
  deriving anyclass (Exception)

data MMToken = MMToken
  { mmtAc        ∷ GYAssetClass,
    mmtPrecision ∷ Natural
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "mmt", LowerFirst]] MMToken

mmtLovelace ∷ MMToken
mmtLovelace = MMToken {mmtAc = GYLovelace, mmtPrecision = 6}

-- TODO: Need to incorporate CIP-67.
-- | Shows the amount of token in it's display units.
--
-- >>> showTokenAmount mmtLovelace 1_000_000
-- "1.000000 ADA"
--
-- >>> showTokenAmount (MMToken gensAC 6) 100_000_000
-- "100.000000 \NUL\DC4\223\DLEGENS"
--
-- >>> showTokenAmount (MMToken frenAC 0) 1_000_000
-- "1000000.0 FREN"
showTokenAmount :: MMToken -> Integer -> String
showTokenAmount MMToken {..} amt = case someNatVal mmtPrecision of
  SomeNat (_ :: Proxy n) ->
    let fx :: Fixed (10 ^ n) = MkFixed amt
    in showFixed False fx <> " " <> getTokenName mmtAc
  where
    getTokenName GYLovelace                   = "ADA"
    getTokenName (GYToken _ (GYTokenName bs)) = unpack bs  -- Using @unpack@ as @show@ on `GYTokenName` leads to superfluous double quotes.

data MMTokenPair = MMTokenPair
  { mmtpCurrency  ∷ MMToken,
    mmtpCommodity ∷ MMToken
  }
  deriving stock (Eq, Ord, Show)

mkMMTokenPair ∷ MMToken → MMToken → MMTokenPair
mkMMTokenPair currSt commSt =
  MMTokenPair
    { mmtpCurrency = currSt,
      mmtpCommodity = commSt
    }

toOAPair ∷ MMTokenPair → OrderAssetPair
toOAPair MMTokenPair {mmtpCurrency, mmtpCommodity} =
  OAssetPair
    { currencyAsset = mmtAc mmtpCurrency,
      commodityAsset = mmtAc mmtpCommodity
    }

data MaestroPairOverride = MaestroPairOverride
  { mpoPair             ∷ !String,
    mpoCommodityIsFirst ∷ !Bool
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] MaestroPairOverride

data PriceConfig = PriceConfig
  { pcApiKey     ∷ !(Confidential Text),
    pcResolution ∷ !Resolution,
    pcNetworkId  ∷ !GYNetworkId,
    pcDex        ∷ !Dex,
    pcOverride   ∷ !(Maybe MaestroPairOverride)
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] PriceConfig

data MaestroPP = MaestroPP
  { mppEnv        ∷ !(MaestroEnv 'V1),
    mppResolution ∷ !Resolution,
    mppDex        ∷ !Dex,
    mppOverride   ∷ !(Maybe MaestroPairOverride)
  }

data PricesProviders = PP
  { maestroPP   ∷ !MaestroPP,
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
          mppResolution = pcResolution,
          mppDex = pcDex,
          mppOverride = pcOverride
        }

{-
    It contains: * Total sell volume in commodity asset
                 * Total buy volume in commodity asset

-}
data OBMarketTokenInfo = OBMarketTokenInfo
  { mtSellVol ∷ !Natural,
    mtBuyVol  ∷ !Natural
  }
  deriving stock (Show)

type OBMarketInfo = M.Map MMTokenPair OBMarketTokenInfo

mkOBMarketTokenInfo
  ∷ Price
  → Spread
  → Orders 'SellOrder
  → Orders 'BuyOrder
  → OBMarketTokenInfo
mkOBMarketTokenInfo (Price marketPrice) Spread {..} sellOrders buyOrders =
  OBMarketTokenInfo
    { mtSellVol = volumeMax sumVolSell,
      mtBuyVol = floor $ toRational (volumeMax sumVolBuy) * marketPrice
    }
 where
  sumVolSell ∷ Volume
  sumVolSell = volumeLTPrice (Price (marketPrice + (marketPrice * sellSideSpread))) sellOrders

  sumVolBuy ∷ Volume
  sumVolBuy = volumeGTPrice (Price (marketPrice - (marketPrice * buySideSpread))) buyOrders

getOrderBookPrices
  ∷ PricesProviders
  → [MMTokenPair]
  → Price
  → Spread
  → IO (OBMarketInfo, MultiAssetOrderBook)
getOrderBookPrices PP {orderBookPP = (c, dex)} mmts price priceCheckSpread = do
  maOrderBook ← populateOrderBook c dex (dexPORefs dex) (map toOAPair mmts)
  return (M.fromList $ map buildPrice $ maOrderBookToList maOrderBook, maOrderBook)
 where
  buildPrice ∷ (OrderAssetPair, OrderBook) → (MMTokenPair, OBMarketTokenInfo)
  buildPrice (oap, ob) =
    let mmtPair = toMMTPair oap
        sndElement = uncurry (mkOBMarketTokenInfo price priceCheckSpread) . (sellOrders &&& buyOrders) $ ob
     in (mmtPair, sndElement)

  toMMTPair ∷ OrderAssetPair → MMTokenPair
  toMMTPair OAssetPair {currencyAsset, commodityAsset} =
    find (\MMTokenPair {..} → mmtAc mmtpCurrency == currencyAsset && mmtAc mmtpCommodity == commodityAsset) mmts
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
  → MMTokenPair
  → IO Price
getMaestroPrice PP {maestroPP = MaestroPP {..}} mmtp = do
  (pairName, commodityIsA) ← case mppOverride of
    -- We have to override with given details.
    Just (MaestroPairOverride {..}) → do
      pure (pack mpoPair, mpoCommodityIsFirst)
    -- We are given commodity token and need to find pair name.
    Nothing → do
      allDexPairs ← dexPairResponsePairs <$> (handleMaestroError (functionLocationIdent <> " - fetching dex pairs") <=< try $ pairsFromDex mppEnv mppDex)
      -- TODO: Remove it once Maestro is able to return for it.
      let adaUsdmPair =
            DexPairInfo
              { dexPairInfoCoinBPolicy = "c48cbb3d5e57ed56e276bc45f99ab39abe94e6cd7ac39fb402da47ad",
                dexPairInfoCoinBAssetName = "0014df105553444d",
                dexPairInfoCoinAPolicy = "",
                dexPairInfoCoinAAssetName = "",
                dexPairInfoPair = "ADA-USDM"
              }
      let go []           = throwIO MaestroPairNotFound
          go (dpi : dpis) = maybe (go dpis) pure $ isRelevantPairInfo dpi
      first dexPairInfoPair <$> go (adaUsdmPair : allDexPairs)

  let pair = TaggedText pairName

  ohlInfo ← handleMaestroError (functionLocationIdent <> " - fetching price from pair") <=< try $ pricesFromDex mppEnv mppDex pair (Just mppResolution) Nothing Nothing Nothing (Just Descending)

  let info = head ohlInfo
      curPrecision :: Int = fromIntegral $ mmtPrecision $ mmtpCurrency mmtp
      comPrecision :: Int = fromIntegral $ mmtPrecision $ mmtpCommodity mmtp
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
        <$ findMatchingMMTP
          (dexPairInfoCoinAAssetName, dexPairInfoCoinAPolicy)
          (dexPairInfoCoinBAssetName, dexPairInfoCoinBPolicy)
    )
      <|> ( (dpi, True)
              <$ findMatchingMMTP
                (dexPairInfoCoinBAssetName, dexPairInfoCoinBPolicy)
                (dexPairInfoCoinAAssetName, dexPairInfoCoinAPolicy)
          )

  findMatchingMMTP ∷ (TokenName, PolicyId) → (TokenName, PolicyId) → Maybe MMTokenPair
  findMatchingMMTP tokenA tokenB = fromRight Nothing $ do
    assetClassA ← assetClassFromMaestro tokenA
    assetClassB ← assetClassFromMaestro tokenB
    Right $ if assetClassA == mmtAc (mmtpCurrency mmtp) && assetClassB == mmtAc (mmtpCommodity mmtp) then Just mmtp else Nothing

  functionLocationIdent = "getMaestroPrice"
