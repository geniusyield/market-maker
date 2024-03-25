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
import           Data.Either                               (fromRight, rights)
import           Data.Fixed                                (Fixed (..),
                                                            showFixed)
import           Data.Function                             ((&))
import           Data.List                                 (find)
import qualified Data.List.NonEmpty                        as NE
import qualified Data.Map.Strict                           as M
import           Data.Maybe                                (fromJust)
import           Data.Ratio                                ((%))
import           Data.Text                                 (Text, pack)
import           Deriving.Aeson
import           GeniusYield.GYConfig
import           GeniusYield.Imports                       (Proxy)
import           GeniusYield.MarketMaker.Orphans           ()
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
                                                            Price (..), invPrice,
                                                            Volume (..))
import           GeniusYield.Providers.Common              (silenceHeadersClientError)
import           GeniusYield.Providers.Maestro
import           GeniusYield.Types
import           GHC.TypeLits                              (type (^))
import           GHC.TypeNats                              (SomeNat (SomeNat),
                                                            someNatVal)
import           Maestro.Client.V1
import           Maestro.Types.V1                          as Maestro
import           Servant.Client                            (mkClientEnv)

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import GeniusYield.Types
-- >>> let gensAC = "dda5fdb1002f7389b33e036b6afee82a8189becb6cba852e8b79b4fb.0014df1047454e53"
-- >>> let frenAC = "fc11a9ef431f81b837736be5f53e4da29b9469c983d07f321262ce61.4652454e"


-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

data PriceConfigV2 = PriceConfigV2
  { pcPriceCommonCfg     ∷ !PriceCommonCfg,
    pcPricesProviderCfgs ∷ !(NE.NonEmpty PricesProviderCfg)
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] PriceConfigV2

data PriceCommonCfg = PriceCommonCfg
  { pccCommonResolution   ∷ !CommonResolution,
    pccNetworkId          ∷ !GYNetworkId,
    pccPriceDiffThreshold ∷ !Double
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] PriceCommonCfg

data PricesProviderCfg =
    MaestroConfig
  { mcApiKey             ∷ !(Confidential Text),
    mcResolutionOverride ∷ !(Maybe Maestro.Resolution),
    mcDex                ∷ !Dex,
    mcPairOverride       ∷ !(Maybe MaestroPairOverride)
  }
  | TaptoolsConfig
  { ttcApiKey             ∷ !(Confidential Text),
    ttcResolutionOverride ∷ !(Maybe TtResolution)
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] PricesProviderCfg


-------------------------------------------------------------------------------
-- Price "estimate" from providers
-------------------------------------------------------------------------------

data MMToken = MMToken
  { mmtAc        ∷ GYAssetClass,
    mmtPrecision ∷ Natural
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "mmt", LowerFirst]] MMToken

data MMTokenPair = MMTokenPair
  { mmtpCurrency  ∷ MMToken,
    mmtpCommodity ∷ MMToken
  }
  deriving stock (Eq, Ord, Show)

data MaestroPriceException
  = MaestroPairNotFound
  | MaestroApiError !Text !MaestroError
  deriving stock (Show)
  deriving anyclass (Exception)

data MaestroPairOverride = MaestroPairOverride
  { mpoPair             ∷ !String,
    mpoCommodityIsFirst ∷ !Bool
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] MaestroPairOverride

data PriceError = PriceUnavailable | PriceIsZero | PriceMismatch

newtype GetQuota = GetQuota { getQuota ∷ MMTokenPair → IO (Either PriceError Price) }

buildGetQuota ∷ PriceCommonCfg → PricesProviderCfg → GetQuota

buildGetQuota PriceCommonCfg {..} MaestroConfig {..} = GetQuota $ \mmtp → do
  env ← networkIdToMaestroEnv (coerce mcApiKey) pccNetworkId
  res ← maybe (fromCommonResolution pccCommonResolution) pure mcResolutionOverride

  (pairName, commodityIsA) ← case mcPairOverride of
    -- We have to override with given details.
    Just (MaestroPairOverride {..}) → do
      pure (pack mpoPair, mpoCommodityIsFirst)
    -- We are given commodity token and need to find pair name.
    Nothing → do
      allDexPairs ← dexPairResponsePairs <$> (handleMaestroError (functionLocationIdent <> " - fetching dex pairs") <=< try $ pairsFromDex env mcDex)

      let go []           = throwIO MaestroPairNotFound
          go (dpi : dpis) = maybe (go dpis) pure $ isRelevantPairInfo mmtp dpi
      first dexPairInfoPair <$> go allDexPairs

  let pair = TaggedText pairName

  ohlInfo ← handleMaestroError (functionLocationIdent <> " - fetching price from pair") <=< try $ pricesFromDex env mcDex pair (Just res) (Just Descending)

  let info = head ohlInfo
      curPrecision ∷ Int = fromIntegral $ mmtPrecision $ mmtpCurrency mmtp
      comPrecision ∷ Int = fromIntegral $ mmtPrecision $ mmtpCommodity mmtp
      precisionDiff = 10 ** fromIntegral (curPrecision - comPrecision)

      price =
        if commodityIsA
          then ohlcCandleInfoCoinBClose info
          else ohlcCandleInfoCoinAClose info

      adjustedPrice = price * precisionDiff

  return . Right $ Price (toRational adjustedPrice)

  where
    isRelevantPairInfo ∷ MMTokenPair → DexPairInfo → Maybe (DexPairInfo, Bool)
    isRelevantPairInfo mmtp dpi@DexPairInfo {..} =
      ( (dpi, False)
          <$ findMatchingMMTP mmtp
            (dexPairInfoCoinAAssetName, dexPairInfoCoinAPolicy)
            (dexPairInfoCoinBAssetName, dexPairInfoCoinBPolicy)
      )
        <|> ( (dpi, True)
                <$ findMatchingMMTP mmtp
                  (dexPairInfoCoinBAssetName, dexPairInfoCoinBPolicy)
                  (dexPairInfoCoinAAssetName, dexPairInfoCoinAPolicy)
            )

    findMatchingMMTP ∷ MMTokenPair → (TokenName, PolicyId) → (TokenName, PolicyId) → Maybe MMTokenPair
    findMatchingMMTP mmtp tokenA tokenB = fromRight Nothing $ do
      assetClassA ← assetClassFromMaestro tokenA
      assetClassB ← assetClassFromMaestro tokenB
      Right $ if assetClassA == mmtAc (mmtpCurrency mmtp) && assetClassB == mmtAc (mmtpCommodity mmtp) then Just mmtp else Nothing

    functionLocationIdent = "getMaestroPrice"
    
buildGetQuota PriceCommonCfg {..} TaptoolsConfig {..} = GetQuota $ \mmtp → case pccNetworkId of
  GYMainnet → do
    manager' ← taptoolsManager ttcApiKey
    let env = mkClientEnv manager' taptoolsBaseUrl

    res ← maybe (fromCommonResolution pccCommonResolution) pure ttcResolutionOverride

    let priceFromTokenPair
          | mmtpCurrency mmtp  == mmtLovelace = getAssetPrice . mmtAc . mmtpCommodity $ mmtp
          | mmtpCommodity mmtp == mmtLovelace = do
              currencyPrice ← getAssetPrice . mmtAc . mmtpCurrency $ mmtp 
              return $ invPrice currencyPrice
          | otherwise = throwIO $ userError "Trading commodity pairs (non-ADA) not yet supported."
          where
            getAssetPrice ∷ GYAssetClass → IO Price
            getAssetPrice gyAsset = case gyAsset of
                GYLovelace   → return $ Price (1 % 1)
                GYToken cs _ → do
                  let unit     = show cs
                      interval = show res
                  ohlcvInfo ← priceFromTaptools (Just unit) (Just interval) (Just 1) env
                  case ohlcvInfo of
                    Left e         → throwIO e
                    Right ttOHLCVs → return (Price . toRational . close . head $ ttOHLCVs)
                    
    p ← priceFromTokenPair
    if getPrice p == 0 then return . Left  $ PriceIsZero
                       else return . Right $ p

  _         → return . Left $ PriceUnavailable

buildGetQuotas ∷ PriceConfigV2 → [GetQuota]
buildGetQuotas PriceConfigV2 {..} = buildGetQuota pcPriceCommonCfg <$> NE.toList pcPricesProviderCfgs

priceEstimate ∷ PriceConfigV2 → MMTokenPair → IO (Either PriceError Price)
priceEstimate pc@PriceConfigV2 {..} mmtp = do
  let providers = buildGetQuotas pc
  eps ← mapM (\prov → getQuota prov mmtp) providers
  
  case rights eps of
    []   → throwIO $ userError "Price not available from any provider."
    ps → do
      let p   = Price . mean $ getPrice <$> ps
          rsd = relStdDev $ fromRational . getPrice <$> ps
          ths = pccPriceDiffThreshold pcPriceCommonCfg
      if rsd > ths
        then return . Left $ PriceMismatch
        else return . Right $ p


-------------------------------------------------------------------------------
-- Order Book Prices and other Helper Functions
-------------------------------------------------------------------------------

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
showTokenAmount ∷ MMToken → Integer → String
showTokenAmount MMToken {..} amt = case someNatVal mmtPrecision of
  SomeNat (_ ∷ Proxy n) ->
    let fx ∷ Fixed (10 ^ n) = MkFixed amt
    in showFixed False fx <> " " <> getTokenName mmtAc
  where
    getTokenName GYLovelace                   = "ADA"
    getTokenName (GYToken _ (GYTokenName bs)) = unpack bs  -- Using @unpack@ as @show@ on `GYTokenName` leads to superfluous double quotes.

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
  → [MMTokenPair]
  → Price
  → Rational
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


-------------------------------------------------------------------------------
-- Deprecated
-------------------------------------------------------------------------------

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

--ToDo:  Write in terms of 'buildGetQuota PriceCommonCfg {..} MaestroConfig {..}' to avoid code duplication.
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
      first dexPairInfoPair <$> go allDexPairs

  let pair = TaggedText pairName

  ohlInfo ← handleMaestroError (functionLocationIdent <> " - fetching price from pair") <=< try $ pricesFromDex mppEnv mppDex pair (Just mppResolution) (Just Descending)

  let info = head ohlInfo
      curPrecision ∷ Int = fromIntegral $ mmtPrecision $ mmtpCurrency mmtp
      comPrecision ∷ Int = fromIntegral $ mmtPrecision $ mmtpCommodity mmtp
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
