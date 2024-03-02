module GeniusYield.MarketMaker.Constants (
  awaitTxParams,
  logNS,
  makerFeeRatio,
) where

import Data.Ratio ((%))
import GeniusYield.Types

awaitTxParams ∷ GYAwaitTxParameters
awaitTxParams = GYAwaitTxParameters {maxAttempts = 25, confirmations = 1, checkInterval = 10_000_000}

logNS ∷ GYLogNamespace
logNS = "MM"

-- TODO: Get it from blockchain instead. Note that this is only used to determine funds needed by wallet and is not forwarded to dex-contracts-api library.
makerFeeRatio ∷ Rational
makerFeeRatio = 3 % 1000 -- Is 0.3%, so ratio should be 0.003 == 3 / 1000.
