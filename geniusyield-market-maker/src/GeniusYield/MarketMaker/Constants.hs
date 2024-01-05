module GeniusYield.MarketMaker.Constants (
  awaitTxParams,
  logNS,
  makerFeeRatio,
) where

import Data.Ratio ((%))
import GeniusYield.Types

awaitTxParams ∷ GYAwaitTxParameters
awaitTxParams = GYAwaitTxParameters {maxAttempts = 20, confirmations = 1, checkInterval = 10_000_000}

logNS ∷ GYLogNamespace
logNS = "MM"

-- TODO: Get it from blockchain instead.
makerFeeRatio ∷ Rational
makerFeeRatio = 3 % 1000 -- Is 0.3%, so ratio should be 0.003 == 3 / 1000.
