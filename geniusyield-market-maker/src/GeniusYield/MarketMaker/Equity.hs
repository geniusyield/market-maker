{-|
Module      : GeniusYield.MarketMaker.Equity
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}

module GeniusYield.MarketMaker.Equity
  ( Equity
  , equityFromValue
  , equityToValue
  , showEquity
  , normalizeEquity
  ) where

import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import           GeniusYield.Imports            (coerce)
import           GeniusYield.MarketMaker.Prices (MMToken (..), showTokenAmount)
import           GeniusYield.OrderBot.Types     (Price (..))
import           GeniusYield.Types              (GYAssetClass (..), GYValue,
                                                 valueToMap)
import           GHC.Natural                    (Natural)

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import GeniusYield.MarketMaker.Prices (mmtLovelace)
-- >>> import GeniusYield.Types
-- >>> import qualified Data.Set as Set
-- >>> let gensAC = "dda5fdb1002f7389b33e036b6afee82a8189becb6cba852e8b79b4fb.0014df1047454e53"
-- >>> let frenAC = "fc11a9ef431f81b837736be5f53e4da29b9469c983d07f321262ce61.4652454e"
-- >>> let gensMMToken = MMToken gensAC 6
-- >>> let frenMMToken = MMToken frenAC 0
-- >>> tokensSet = Set.fromList [mmtLovelace, gensMMToken, frenMMToken]

-- | Opaque interface to denote equity of bot which is a bag of tokens.
newtype Equity = Equity GYValue
  deriving newtype (Semigroup, Monoid)

equityFromValue :: GYValue -> Equity
equityFromValue = coerce

equityToValue :: Equity -> GYValue
equityToValue = coerce

-- | Displays amounts of all assets in equity using `showTokenAmount`.
--
-- >>> showEquity (equityFromValue $ valueFromLovelace 1_000_000 <> valueSingleton gensAC 1_000_000 <> valueSingleton frenAC 1_000) tokensSet
-- "\n\t1.000000 ADA\n\t1.000000 \NUL\DC4\223\DLEGENS\n\t1000.0 FREN"
showEquity :: Equity -> Set.Set MMToken -> String
showEquity (valueToMap . equityToValue -> eqValMap) tokenDetails = Map.foldlWithKey' f mempty eqValMap
  where
    acToPrecision = Set.foldl' (\accum MMToken {..} -> Map.insert mmtAc mmtPrecision accum) mempty tokenDetails
    f :: String -> GYAssetClass -> Integer -> String
    f accum ac amnt =
      case Map.lookup ac acToPrecision of
        Nothing -> accum
        Just pr -> accum <> "\n\t" <> showTokenAmount (MMToken ac pr) amnt

-- | Normalize equity to get a representation in terms of single asset.
normalizeEquity :: Equity -> Map GYAssetClass Price -> Natural
normalizeEquity (valueToMap . equityToValue -> eqValMap) acPriceMap = Map.foldlWithKey' f 0 eqValMap
  where
    f accum ac amnt = case Map.lookup ac acPriceMap of
      Nothing -> accum
      Just pr -> accum + floor (fromIntegral amnt * getPrice pr)
