{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module GeniusYield.MarketMaker.Prices where

import           Control.Applicative                       ((<|>))
import           Control.Arrow                             (Arrow (first),
                                                            (&&&))
import           Control.Exception                         (Exception, throwIO,
                                                            try)
import           Control.Monad                             ((<=<))
import           Data.Aeson                                (parseJSON)
import           Data.ByteString.Char8                     (unpack)
import           Data.Coerce                               (coerce)
import           Data.Either                               (fromRight, lefts, rights)
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
import           GeniusYield.Imports                       (Proxy, when)
import           GeniusYield.MarketMaker.Orphans           ()
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

-- | PriceConfigV1 is deprecated
data PriceConfigV1 = PriceConfigV1
  { pcApiKey     ∷ !(Confidential Text),
    pcResolution ∷ !Resolution,
    pcNetworkId  ∷ !GYNetworkId,
    pcDex        ∷ !Dex,
    pcOverride   ∷ !(Maybe MaestroPairOverride)
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] PriceConfigV1

-- | Needed to parse deprecated 'PriceConfigV1'
defaultMaestroFailDelay ∷ Int
defaultMaestroFailDelay = 100  -- TODO: what is a reasonable value to put here?

data PriceConfigV2 = PriceConfigV2
  { pcPriceCommonCfg     ∷ !PriceCommonCfg,
    pcPricesProviderCfgs ∷ !(NE.NonEmpty PricesProviderCfg)
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] PriceConfigV2

data PriceCommonCfg = PriceCommonCfg
  { pccCommonResolution       ∷ !CommonResolution,
    pccNetworkId              ∷ !GYNetworkId,
    pccPricesProvidersWeights ∷ ![Rational],  -- ^ Corresponding to each @PricesProviderCfg@
    pccPriceDiffThreshold1    ∷ !Double,  -- ^ Triggers "mildly spooked"
    pccPriceDiffThreshold2    ∷ !Double,  -- ^ Triggers "very spooked"; Threshold2 >= Threshold1
    pccPriceDiffDelay1        ∷ !Int,
    pccPriceDiffDelay2        ∷ !Int
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

data PriceConfig = PCVersion1 PriceConfigV1 | PCVersion2 PriceConfigV2  deriving stock Show

instance FromJSON PriceConfig where
  parseJSON v = (PCVersion1 <$> parseJSON v) <|> (PCVersion2 <$> parseJSON v)


-------------------------------------------------------------------------------
-- Build Prices Providers, Quotas and Weights
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

data PricesProvidersAggregator = PPA
  { ppaCommonCfg     ∷ !PriceCommonCfg,
    ppaPricesCluster ∷ ![PricesProviderCfg]
  }

data PricesProviders = PP
  { pricesAggregatorPP ∷ !PricesProvidersAggregator,
    orderBookPP        ∷ !(Connection, DEXInfo)
  }

buildPP
  ∷ Connection
  → DEXInfo
  → PriceConfig
  → IO PricesProviders
buildPP c dex pc =
  PP
   <$> ppPricesAggregator
   <*> return (c, dex)
 where
  ppPricesAggregator ∷ IO PricesProvidersAggregator
  ppPricesAggregator = case pc of
    PCVersion1 (PriceConfigV1 {..}) → do
      let pccNetworkId   = pcNetworkId
          mcApiKey       = pcApiKey
          mcDex          = pcDex
          mcPairOverride = pcOverride
      return PPA
        { ppaCommonCfg = PriceCommonCfg
          { pccCommonResolution       = CRes5m  -- placeholder, unused
          , pccNetworkId              = pccNetworkId
          , pccPricesProvidersWeights = [1]
          , pccPriceDiffThreshold1    = 1
          , pccPriceDiffThreshold2    = 1
          , pccPriceDiffDelay1        = 0       -- placeholder, unused
          , pccPriceDiffDelay2        = defaultMaestroFailDelay
          }

        , ppaPricesCluster = [ MaestroConfig
            { mcApiKey = mcApiKey
            , mcResolutionOverride = Just pcResolution
            , mcDex = mcDex
            , mcPairOverride = mcPairOverride
            } ]
        }

    PCVersion2 (PriceConfigV2 {..}) → do
      let ppaCommonCfg     = pcPriceCommonCfg
          thresHold1       = pccPriceDiffThreshold1 ppaCommonCfg
          thresHold2       = pccPriceDiffThreshold1 ppaCommonCfg
          ppaPricesCluster = NE.toList pcPricesProviderCfgs

      when (length (pccPricesProvidersWeights ppaCommonCfg) /= length ppaPricesCluster) $
        throwIO $ userError "Expected 'pccPricesProvidersWeights' to be of same length as 'pcPricesProviderCfgs'."
      when (thresHold2 < thresHold1) $
        throwIO $ userError "Expected 'pccPriceDiffThreshold2 >= pccPriceDiffThreshold1'."

      return PPA
        { ppaCommonCfg     = ppaCommonCfg,
          ppaPricesCluster = ppaPricesCluster
        }
  
data SourceError = SourceUnavailable
  deriving stock Show

data PriceError = PriceMismatch1 | PriceMismatch2 | PriceUnavailable | PriceSourceFail Price
  deriving stock Show

newtype GetQuota = GetQuota { getQuota ∷ MMTokenPair → IO (Either SourceError Price) }

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
              return $ Right invPrice <*> currencyPrice
          | otherwise = throwIO $ userError "Trading commodity pairs (non-ADA) not yet supported."
          where
            getAssetPrice ∷ GYAssetClass → IO (Either SourceError Price)
            getAssetPrice gyAsset = case gyAsset of
                GYLovelace   → return . Right $ Price (1 % 1)
                GYToken cs _ → do
                  let unit     = show cs
                      interval = show res
                  ohlcvInfo ← priceFromTaptools (Just unit) (Just interval) (Just 1) env
                  case ohlcvInfo of
                    Left _         → return (Left SourceUnavailable)
                    Right ttOHLCVs → return (Right . Price . toRational . close . head $ ttOHLCVs)
    priceFromTokenPair

  _         → throwIO $ userError "Price unavailable."

buildGetQuotas ∷ PricesProviders → [GetQuota]
buildGetQuotas PP {pricesAggregatorPP = PPA {..}} = buildGetQuota ppaCommonCfg <$> ppaPricesCluster

buildWeights ∷ PricesProviders → [Rational]
buildWeights PP {pricesAggregatorPP = PPA {..}} = pccPricesProvidersWeights ppaCommonCfg


-------------------------------------------------------------------------------
-- Price "estimate" from providers
-------------------------------------------------------------------------------

priceEstimate ∷ PricesProviders → MMTokenPair → IO (Either PriceError Price)
priceEstimate pp mmtp = do
  let pproviders = buildGetQuotas pp
      weights    = buildWeights pp

  eps ← mapM (\prov → getQuota prov mmtp) pproviders

  let hasSourceFailed = not . null $ [ s | s@SourceUnavailable ← lefts eps ]
  
  case rights eps of
    [] → return $ Left PriceUnavailable
    ps → do
      let p    = Price . weightedMean weights $ getPrice <$> ps
          rsd  = relStdDev $ fromRational . getPrice <$> ps
          cCfg = ppaCommonCfg . pricesAggregatorPP $ pp
          ths1 = pccPriceDiffThreshold1 cCfg
          ths2 = pccPriceDiffThreshold2 cCfg

          priceAnalyzed
            | rsd > ths2      = Left PriceMismatch2
            | rsd > ths1      = Left PriceMismatch1
            | hasSourceFailed = Left $ PriceSourceFail p
            | otherwise       = Right p

      return priceAnalyzed


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


