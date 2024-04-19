{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module GeniusYield.MarketMaker.Prices where

import           Control.Applicative                       ((<|>))
import           Control.Arrow                             (Arrow (first),
                                                            (&&&))
import           Control.Exception                         (Exception, throwIO,
                                                            try, catch, IOException, displayException, evaluate)
import           Control.Monad                             ((<=<))
import           Data.Aeson                                (parseJSON)
import           Data.ByteString.Char8                     (unpack)
import           Data.Coerce                               (coerce)
import           Data.Either                               (fromRight)
import           Data.Fixed                                (Fixed (..),
                                                            showFixed)
import           Data.Function                             ((&))
import           Data.List                                 (find)
import qualified Data.List.NonEmpty                        as NE
import qualified Data.Map.Strict                           as M
import           Data.Maybe                                (fromJust, catMaybes)
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
                                                            Price (..), Volume (..))
import           GeniusYield.Providers.Common              (silenceHeadersClientError)
import           GeniusYield.Providers.Maestro
import           GeniusYield.Types
import           GHC.TypeLits                              (type (^))
import           GHC.TypeNats                              (SomeNat (SomeNat),
                                                            someNatVal)
import           Maestro.Client.V1
import           Maestro.Types.V1                          as Maestro
import           Servant.Client                            (mkClientEnv)
import           System.IO                                 (withFile, IOMode(ReadMode), hGetContents)


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
    pccPricesProvidersWeights ∷ ![Int],   -- ^ Corresponding to each @PricesProviderCfg@
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
      mcPairOverride       ∷ !(Maybe MaestroPairOverride),
      mcOffsetFilePath     ∷ !(Maybe FilePath),  -- TESTING
      mcAvailablePath      ∷ !(Maybe FilePath)   -- TESTING
    }
  | TaptoolsConfig
    { ttcApiKey             ∷ !(Confidential Text),
      ttcResolutionOverride ∷ !(Maybe TtResolution),
      ttcPairOverride       ∷ !(Maybe TaptoolsPairOverride),
      ttcAvailablePath      ∷ !(Maybe FilePath)  -- TESTING
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

data TaptoolsPairOverride = TaptoolsPairOverride
  { ttpoAsset            ∷ !String,
    ttpoPrecision        ∷ !Natural,
    ttpoCommodityIsFirst ∷ !Bool
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] TaptoolsPairOverride

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
            { mcApiKey             = mcApiKey
            , mcResolutionOverride = Just pcResolution
            , mcDex                = mcDex
            , mcPairOverride       = mcPairOverride
            , mcOffsetFilePath     = Nothing  -- TESTING
            , mcAvailablePath      = Nothing  -- TESTING
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

data PriceIndicator = PriceMismatch1 | PriceMismatch2 | PriceUnavailable | PriceSourceFail Price | PriceAverage Price
  deriving stock Show

newtype GetQuota = GetQuota { getQuota ∷ MMTokenPair → IO (Either SourceError Price) }

buildGetQuota ∷ PriceCommonCfg → PricesProviderCfg → GetQuota

buildGetQuota PriceCommonCfg {..} MaestroConfig {..} = GetQuota $ \mmtp → do
  maestroAvailable ← case mcAvailablePath of  -- TESTING
    Nothing   → pure True
    Just path → readBool path `catch` handleReadError

  if not maestroAvailable then return $ Left SourceUnavailable else do
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

    offset ← case mcOffsetFilePath of  -- TESTING
      Nothing   → pure 0
      Just path → readOffset path `catch` handleReadError

    let info = head ohlInfo
        curPrecision ∷ Int = fromIntegral $ mmtPrecision $ mmtpCurrency mmtp
        comPrecision ∷ Int = fromIntegral $ mmtPrecision $ mmtpCommodity mmtp
        precisionDiff = 10 ** fromIntegral (curPrecision - comPrecision)

        price =
          if commodityIsA
            then ohlcCandleInfoCoinBClose info
            else ohlcCandleInfoCoinAClose info

        adjustedPrice = price * precisionDiff + offset

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
    taptoolsAvailable ← case ttcAvailablePath of  -- TESTING
      Nothing   → pure True
      Just path → readBool path `catch` handleReadError
        
    if not taptoolsAvailable then return $ Left SourceUnavailable else do
      (commodity ∷ Either (String, Natural) MMToken, commodityIsFirst ∷ Bool) ← 
        case ttcPairOverride of
          Just ttpo                             → pure (Left (ttpoAsset ttpo, ttpoPrecision $ ttpo), ttpoCommodityIsFirst ttpo)
          Nothing
            | mmtpCurrency mmtp  == mmtLovelace → pure (Right $ mmtpCommodity mmtp, False)
            | mmtpCommodity mmtp == mmtLovelace → pure (Right $ mmtpCurrency mmtp, True)
            | otherwise                         → throwIO $ userError "Trading commodity pairs (non-ADA) not yet supported."

      manager' ← taptoolsManager ttcApiKey
      let env = mkClientEnv manager' taptoolsBaseUrl

      res ← maybe (fromCommonResolution pccCommonResolution) pure ttcResolutionOverride
      let interval = show res

      case commodity of
        Right MMToken { mmtAc = GYLovelace } → return . Right $ Price (1 % 1)
        _                                    → do
          let (unit, precision) = case commodity of
                Left (s, n)                                 → (s, n)
                Right MMToken { mmtAc = GYToken cs tn, .. } → (show cs ++ show tn, fromIntegral mmtPrecision)

          ohlcvInfo ← priceFromTaptools (Just unit) (Just interval) (Just 1) env

          case ohlcvInfo of
            Left _   → return $ Left SourceUnavailable
            Right [] → return $ Left SourceUnavailable
            Right (ttOHLCV : _) → do
              let price'        = close ttOHLCV
                  price         = if commodityIsFirst then 1 / price' else price'
                  precisionDiff = 10 ** fromIntegral (mmtPrecision mmtLovelace - precision)
                  adjustedPrice = price * precisionDiff

              return . Right . Price . toRational $ adjustedPrice

  _         → throwIO $ userError "Price unavailable."

buildGetQuotas ∷ PricesProviders → [GetQuota]
buildGetQuotas PP {pricesAggregatorPP = PPA {..}} = buildGetQuota ppaCommonCfg <$> ppaPricesCluster

buildWeights ∷ PricesProviders → [Int]
buildWeights PP {pricesAggregatorPP = PPA {..}} = pccPricesProvidersWeights ppaCommonCfg


-------------------------------------------------------------------------------
-- Price "estimate" from providers
-------------------------------------------------------------------------------

priceEstimate ∷ PricesProviders → MMTokenPair → IO PriceIndicator
priceEstimate pp mmtp = do
  let pproviders = buildGetQuotas pp
      weights    = buildWeights pp

  eps ← mapM (\prov → getQuota prov mmtp) pproviders
  
  let weightedPrices = catMaybes $ zipWith matchRight weights eps

  let hasSourceFailed = length weightedPrices < length eps
  
  case weightedPrices of
    []  → return PriceUnavailable
    wps → do
      let p    = Price $ weightedMean wps
          rsd  = relStdDev $ fromRational . snd <$> wps
          cCfg = ppaCommonCfg . pricesAggregatorPP $ pp
          ths1 = pccPriceDiffThreshold1 cCfg
          ths2 = pccPriceDiffThreshold2 cCfg

          priceAnalyzed
            | rsd > ths2      = PriceMismatch2
            | rsd > ths1      = PriceMismatch1
            | hasSourceFailed = PriceSourceFail p
            | otherwise       = PriceAverage p

      return priceAnalyzed

matchRight ∷ Int → Either SourceError Price → Maybe (Rational, Rational)
matchRight w (Right p) = Just (fromIntegral w, getPrice p)
matchRight _ _         = Nothing


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

-------------------------------------------------------------------------------
-- Testing Helper Functions
-------------------------------------------------------------------------------

readOffset ∷ FilePath → IO Double
readOffset filePath = withFile filePath ReadMode $ \handle → do
  contents ← hGetContents handle
  -- Forces actual read
  _ ← evaluate (length contents)
  putStrLn "Testing 'offset' flag:"
  putStrLn contents
  let parsed = reads contents ∷ [(Double, String)]
  case parsed of
      [(num, _)] → return num
      _          → error "could not read number of type 'Double'"

readBool ∷ FilePath → IO Bool
readBool filePath = withFile filePath ReadMode $ \handle → do
  contents ← hGetContents handle
  -- Forces actual read
  _ ← evaluate (length contents)
  putStrLn "Testing 'bool' flag:"
  putStrLn contents
  let parsed = reads contents ∷ [(Bool, String)]
  case parsed of
    [(bool, _)] → return bool
    _           → error "could not read boolean"

handleReadError ∷ forall a. IOException → IO a
handleReadError e = error $ displayException e
