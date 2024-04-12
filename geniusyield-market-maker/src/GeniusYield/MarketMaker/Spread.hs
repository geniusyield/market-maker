module GeniusYield.MarketMaker.Spread (Spread (..)) where

import           Control.Applicative           ((<|>))
import           Data.Aeson                    (FromJSON (..))
import qualified Data.Aeson                    as Aeson
import           Deriving.Aeson
import           GeniusYield.MarketMaker.Utils (camelToSnake)

data Spread = Spread
  { buySideSpread  :: !Rational,
    sellSideSpread :: !Rational
  }
  deriving stock (Show, Generic)
  deriving (ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] Spread

-- |
--
-- >>> Aeson.decode @Spread "{\"numerator\":41,\"denominator\":10000}"
-- Just (Spread {buySideSpread = 41 % 10000, sellSideSpread = 41 % 10000})
--
-- >>> Aeson.encode $ Spread 0.01 0.02
-- "{\"buy_side_spread\":{\"numerator\":1,\"denominator\":100},\"sell_side_spread\":{\"numerator\":1,\"denominator\":50}}"
--
instance FromJSON Spread where
  parseJSON v =
    Aeson.genericParseJSON (Aeson.defaultOptions {Aeson.fieldLabelModifier = camelToSnake}) v
      <|> ( do
              n ← parseJSON v
              pure $ Spread n n
          )

instance Num Spread where
  Spread a b + Spread c d = Spread (a + c) (b + d)
  Spread a b - Spread c d = Spread (a - c) (b - d)
  Spread a b * Spread c d = Spread (a * c) (b * d)
  abs (Spread a b) = Spread (abs a) (abs b)
  signum (Spread a b) = Spread (signum a) (signum b)
  fromInteger n = Spread (fromInteger n) (fromInteger n)
  negate (Spread a b) = Spread (negate a) (negate b)
