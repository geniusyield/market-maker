{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module GeniusYield.MarketMaker.Prices where

import           Control.Applicative                       ((<|>))
import           Control.Arrow                             (Arrow (first),
                                                            (&&&))
import           Control.Concurrent.MVar
import           Control.Exception                         (Exception,
                                                            displayException,
                                                            throwIO, try, handle)
import           Control.Monad                             ((<=<))
import           Data.Aeson                                (parseJSON, ToJSON (..))
import           Data.ByteString.Char8                     (unpack)
import           Data.Coerce                               (coerce)
import           Data.Either                               (fromRight, lefts)
import           Data.Fixed                                (Fixed (..),
                                                            showFixed)
import           Data.Function                             ((&))
import           Data.List                                 (find)
import           Data.List.NonEmpty                        (NonEmpty (..))
import qualified Data.List.NonEmpty                        as NE
import qualified Data.Map.Strict                           as M
import           Data.Maybe                                (catMaybes, fromJust)
import           Data.Ratio                                ((%))
import           Data.Text                                 (Text, pack)
import           Deriving.Aeson
import           GeniusYield.Api.Dex.Constants             (DEXInfo (..))
import           GeniusYield.GYConfig
import           GeniusYield.Imports                       (Proxy, when)
import           GeniusYield.MarketMaker.Constants         (logNS)
import           GeniusYield.MarketMaker.Prices.Taptools
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
import           Maestro.Types.V1                          as Maestro
import           Servant.API                               (ToHttpApiData(toUrlPiece))
import           Servant.Client                            (ClientEnv)


-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import GeniusYield.Types
-- >>> let gensAC = "dda5fdb1002f7389b33e036b6afee82a8189becb6cba852e8b79b4fb.0014df1047454e53"
-- >>> let frenAC = "fc11a9ef431f81b837736be5f53e4da29b9469c983d07f321262ce61.4652454e"


-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

-- TODO: https://github.com/geniusyield/market-maker/issues/86.
deriving newtype instance (ToJSON a) => ToJSON (Confidential a)

-- | PriceConfigV1 is deprecated
data PriceConfigV1 = PriceConfigV1
  { pcApiKey     ∷ !(Confidential Text),
    pcResolution ∷ !Resolution,
    pcNetworkId  ∷ !GYNetworkId,
    pcDex        ∷ !Dex,
    pcOverride   ∷ !(Maybe MaestroPairOverride)
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] PriceConfigV1

data PriceConfigV2 = PriceConfigV2
  { pcPriceCommonCfg     ∷ !PriceCommonCfg,
    pcPricesProviderCfgs ∷ !(NonEmpty PricesProviderCfg)
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] PriceConfigV2

data PriceCommonCfg = PriceCommonCfg
  { pccNetworkId              ∷ !GYNetworkId,
    pccPriceDiffThreshold1    ∷ !Double,          -- ^ Triggers "mildly spooked"
    pccPriceDiffThreshold2    ∷ !Double,          -- ^ Triggers "very spooked"; Threshold2 >= Threshold1
    pccRespiteDelayFactor     ∷ !(Maybe Double),  -- ^ Thread delay multiplier determining the respite delay when  "spooked"
    pccAfterExitRelaxAim1     ∷ !Int,             -- ^ Relaxation time (in cycles) to return to normal after "mildly spooked"
    pccAfterExitWorseMax1     ∷ !Int,             -- ^ Waiting time (in cycles) to transition from "mildly" to "very spooked"
    pccAfterExitRelaxAim2     ∷ !Int,             -- ^ Relaxation time (in cycles) to return to normal after "very spooked"
    pccPricesProvidersWeights ∷ !(NonEmpty Int)   -- ^ Corresponding to each @PricesProviderCfg@
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] PriceCommonCfg

data MaestroConfig = MaestroConfig
  { mcApiKey       ∷ !(Confidential Text),
    mcResolution   ∷ !Maestro.Resolution,
    mcDex          ∷ !Dex,
    mcPairOverride ∷ !(Maybe MaestroPairOverride)
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] MaestroConfig

data TaptoolsConfig = TaptoolsConfig
  { ttcApiKey       ∷ !(Confidential Text),
    ttcPairOverride ∷ !(Maybe TaptoolsPairOverride)
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] TaptoolsConfig

data MockConfig = MockConfig  -- For testing
  { mokcName ∷ !String }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] MockConfig

data PricesProviderCfg =
    MaestroPPC MaestroConfig
  | TaptoolsPPC TaptoolsConfig
  | MockPPC MockConfig
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] PricesProviderCfg

data PriceConfig = PCVersion1 PriceConfigV1 | PCVersion2 PriceConfigV2  deriving stock Show

instance FromJSON PriceConfig where
  parseJSON v = (PCVersion1 <$> parseJSON v) <|> (PCVersion2 <$> parseJSON v)
instance ToJSON PriceConfig where
  toJSON (PCVersion1 v) = toJSON v
  toJSON (PCVersion2 v) = toJSON v


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

data MaestroPairOverride = MaestroPairOverride
  { mpoPair             ∷ !String,
    mpoCommodityIsFirst ∷ !Bool
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] MaestroPairOverride

data TaptoolsPairOverride = TaptoolsPairOverride
  { ttpoAsset     ∷ !GYAssetClass,
    ttpoPrecision ∷ !Natural
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] TaptoolsPairOverride

data MaestroPP = MaestroPP
  { mppEnv          ∷ !(MaestroEnv 'V1),
    mppResolution   ∷ !Maestro.Resolution,
    mppDex          ∷ !Dex,
    mppPairOverride ∷ !(Maybe MaestroPairOverride)
  }

data TaptoolsPP = TaptoolsPP
  { ttppEnv          ∷ !ClientEnv,
    ttppPairOverride ∷ !(Maybe TaptoolsPairOverride)
  }

data MockPP = MockPP
  { mokppName  ∷ !String,
    mokppPrice ∷ !(MVar (Maybe Double))
  }

data PricesProviderBuilt =
    MaestroPPB MaestroPP
  | TaptoolsPPB TaptoolsPP
  | MockPPB MockPP

data PricesProvidersAggregator = PPA
  { ppaCommonCfg     ∷ !PriceCommonCfg,
    ppaPricesCluster ∷ !(NonEmpty PricesProviderBuilt)
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
   <$> ppAggregator
   <*> pure (c, dex)
 where
  ppAggregator ∷ IO PricesProvidersAggregator
  ppAggregator = case pc of
    PCVersion1 (PriceConfigV1 {..}) → do
      env ← networkIdToMaestroEnv (coerce pcApiKey) pcNetworkId

      let pccNetworkId    = pcNetworkId
          mppDex          = pcDex
          mppPairOverride = pcOverride

      return PPA
        { ppaCommonCfg = PriceCommonCfg
          { pccNetworkId              = pccNetworkId
          , pccPriceDiffThreshold1    = 1
          , pccPriceDiffThreshold2    = 1
          , pccRespiteDelayFactor     = Nothing
          , pccAfterExitRelaxAim1     = 0               -- placeholder, unused
          , pccAfterExitWorseMax1     = 0               -- placeholder, unused
          , pccAfterExitRelaxAim2     = 1
          , pccPricesProvidersWeights = NE.singleton 1
          }
        , ppaPricesCluster = NE.fromList [ MaestroPPB MaestroPP
            { mppEnv          = env
            , mppResolution   = pcResolution
            , mppDex          = mppDex
            , mppPairOverride = mppPairOverride
            } ]
        }

    PCVersion2 (PriceConfigV2 {..}) → do
      let thresHold1   = pccPriceDiffThreshold1 pcPriceCommonCfg
          thresHold2   = pccPriceDiffThreshold2 pcPriceCommonCfg

      when (length (pccPricesProvidersWeights pcPriceCommonCfg) /= length pcPricesProviderCfgs) $
        throwIO $ userError "Expected 'pccPricesProvidersWeights' to be of same length as 'pcPricesProviderCfgs'."
      when (thresHold2 < thresHold1) $
        throwIO $ userError "Expected 'pccPriceDiffThreshold2 >= pccPriceDiffThreshold1'."

      PPA pcPriceCommonCfg <$> mapM ppBuilder pcPricesProviderCfgs

      where
        ppBuilder ∷ PricesProviderCfg → IO PricesProviderBuilt
        ppBuilder ppc = case ppc of
          MaestroPPC mc  → do
            env ← networkIdToMaestroEnv (coerce $ mcApiKey mc) (pccNetworkId pcPriceCommonCfg)
            return $ MaestroPPB MaestroPP
              { mppEnv          = env
              , mppResolution   = mcResolution mc
              , mppDex          = mcDex mc
              , mppPairOverride = mcPairOverride mc
              }
          TaptoolsPPC tc → do
            when (pccNetworkId pcPriceCommonCfg /= GYMainnet) (throwIO $ userError "Taptools only supports Mainnet.")
            env ← taptoolsEnv $ ttcApiKey tc
            return $ TaptoolsPPB TaptoolsPP
              { ttppEnv          = env
              , ttppPairOverride = ttcPairOverride tc
              }
          MockPPC mokc   → do
            mokppPrice ← newMVar Nothing
            return $ MockPPB MockPP
              { mokppName  = mokcName mokc
              , mokppPrice = mokppPrice
              }

data PriceIndicator = PriceMismatch1 | PriceMismatch2 | PriceUnavailable | PriceSourceFail [PricesProviderException] [String] Price | PriceAverage Price
  deriving stock Show

-- | Prices provider error:  `SourceUnavailable pricesProviderException pricesProvider`.
data SourceError = SourceUnavailable PricesProviderException String
  deriving stock Show

newtype GetQuota = GetQuota { getQuota ∷ MMTokenPair → IO (Either SourceError Price) }

buildGetQuota ∷ PricesProviderBuilt → GetQuota

buildGetQuota (MaestroPPB MaestroPP {..}) = GetQuota $ \mmtp → do
  handle handleMaestroSourceFail $ do
    (pairName, commodityIsA) ← case mppPairOverride of
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
            go (dpi : dpis) = maybe (go dpis) pure $ isRelevantPairInfo mmtp dpi
        first dexPairInfoPair <$> go (adaUsdmPair : allDexPairs)

    let pair = TaggedText pairName

    ohlInfo ← handleMaestroError (functionLocationIdent <> " - fetching price from pair") <=< try $
      pricesFromDex mppEnv mppDex pair (Just mppResolution) Nothing Nothing Nothing (Just Descending)

    let info = head ohlInfo
        curPrecision ∷ Int = fromIntegral $ mmtPrecision $ mmtpCurrency mmtp
        comPrecision ∷ Int = fromIntegral $ mmtPrecision $ mmtpCommodity mmtp
        precisionDiff = 10 ** fromIntegral (curPrecision - comPrecision)

        price =
          if commodityIsA
            then ohlcCandleInfoCoinBClose info
            else ohlcCandleInfoCoinAClose info

        adjustedPrice = price * precisionDiff

    return . Right . Price . toRational $ adjustedPrice
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

buildGetQuota (TaptoolsPPB TaptoolsPP {..}) = GetQuota $ \mmtp → do
  commodity ∷ MMToken ←
    case ttppPairOverride of
      Just ttpo → pure MMToken {mmtAc = ttpoAsset ttpo, mmtPrecision = ttpoPrecision ttpo}
      Nothing
        | mmtpCurrency mmtp  == mmtLovelace → pure . mmtpCommodity $ mmtp
        | mmtpCommodity mmtp == mmtLovelace → pure . mmtpCurrency $ mmtp
        | otherwise                         → throwIO $ userError "Trading commodity pairs (non-ADA) not yet supported."

  case commodity of
    MMToken { mmtAc = GYLovelace }    → do
      return . Right $ Price (1 % 1)

    MMToken { mmtAc = gyt@(GYToken {}), mmtPrecision = precision } → do
      let unit = TtUnit gyt

      priceInfo ← priceFromTaptools unit ttppEnv

      case priceInfo of
        Left e   → return . Left $ SourceUnavailable (PPTaptoolsErr $ TaptoolsClientError e) "Taptools"
        Right ttpm → case M.lookup unit ttpm of
          Nothing -> return . Left $ SourceUnavailable (PPTaptoolsErr $ TaptoolsError $ "Price not found for given unit: " <> toUrlPiece unit) "Taptools"
          Just price -> do
            let precisionDiff = 10 ** fromIntegral (mmtPrecision mmtLovelace - precision)
                adjustedPrice = price * precisionDiff
            return . Right . Price . toRational $ adjustedPrice

buildGetQuota (MockPPB MockPP {..}) = GetQuota $ \_ → do
  mbPrice ← readMVar mokppPrice

  case mbPrice of
    Nothing → return . Left $ SourceUnavailable (PPMockErr $ MockError "Mock prices provider exception.") mokppName
    Just p  → return . Right . Price . toRational $ p

buildGetQuotas ∷ PricesProviders → NonEmpty GetQuota
buildGetQuotas PP {pricesAggregatorPP = PPA {..}} = buildGetQuota <$> ppaPricesCluster

buildWeights ∷ PricesProviders → NonEmpty Int
buildWeights PP {pricesAggregatorPP = PPA {..}} = pccPricesProvidersWeights ppaCommonCfg


-------------------------------------------------------------------------------
-- Price "estimate" from providers
-------------------------------------------------------------------------------

priceEstimate ∷ PricesProviders → MMTokenPair → IO PriceIndicator
priceEstimate pp mmtp = do
  let pproviders = NE.toList $ buildGetQuotas pp
      weights    = NE.toList $ buildWeights pp

  eps ← mapM (`getQuota` mmtp) pproviders

  let weightedPrices = catMaybes $ zipWith matchRight weights eps

  let lps                   = lefts eps
      hasSourceFailed       = not . null $ lps
      unavailableExceptions = map (\(SourceUnavailable e _) → e) lps
      unavailablePProviders = map (\(SourceUnavailable _ pprovider) → pprovider) lps

  case weightedPrices of
    []  → return PriceUnavailable
    wps → do
      let p    = Price . weightedMean . NE.fromList $ wps
          rsd  = relStdDev . NE.fromList $ fromRational . snd <$> wps
          cCfg = ppaCommonCfg . pricesAggregatorPP $ pp
          ths1 = pccPriceDiffThreshold1 cCfg
          ths2 = pccPriceDiffThreshold2 cCfg

          priceAnalyzed
            | rsd > ths2      = PriceMismatch2
            | rsd > ths1      = PriceMismatch1
            | hasSourceFailed = PriceSourceFail unavailableExceptions unavailablePProviders p
            | otherwise       = PriceAverage p

      return priceAnalyzed
  where
    matchRight ∷ Int → Either SourceError Price → Maybe (Rational, Rational)
    matchRight w (Right p) = Just (fromIntegral w, getPrice p)
    matchRight _ _         = Nothing

priceEstimate' ∷ PricesProviders → MMToken → IO PriceIndicator
priceEstimate' pp mmToken = priceEstimate pp mmtp
  where
    mmtp = mkMMTokenPair mmtLovelace mmToken


-------------------------------------------------------------------------------
-- Order Book Prices
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


-------------------------------------------------------------------------------
-- Exceptions and exception-handling helper functions
-------------------------------------------------------------------------------

data MaestroPriceException
  = MaestroPairNotFound
  | MaestroApiError !Text !MaestroError
  deriving stock (Show)
  deriving anyclass (Exception)

data MockPriceException = MockError !Text
  deriving stock Show
  deriving anyclass Exception

data PricesProviderException
  = PPMaestroErr MaestroPriceException
  | PPTaptoolsErr TaptoolsPriceException
  | PPMockErr MockPriceException
  deriving stock Show

instance Exception PricesProviderException where
  displayException (PPMaestroErr err)  = "Maestro fail: " ++ displayException err
  displayException (PPTaptoolsErr err) = "Taptools fail: " ++ displayException err
  displayException (PPMockErr err)     = "Mock fail: " ++ displayException err

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

-- | Utility function to return a `SourceError` if a `MaestroPriceException` is thrown.
handleMaestroSourceFail ∷ MaestroPriceException → IO (Either SourceError a)
handleMaestroSourceFail mpe = pure . Left $ SourceUnavailable (PPMaestroErr mpe) "Maestro"

logPricesProviderFail ∷ GYProviders → [PricesProviderException] → [String] → IO ()
logPricesProviderFail providers es pps = do
  gyLogWarning providers logNS $ "Some prices provider(s) failed: " ++ show pps
  mapM_ (gyLogWarning providers logNS . displayException) es
