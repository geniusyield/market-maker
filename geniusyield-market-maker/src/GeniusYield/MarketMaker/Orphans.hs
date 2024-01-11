{-# OPTIONS_GHC -Wno-orphans #-}

module GeniusYield.MarketMaker.Orphans where

import           Deriving.Aeson
import           GeniusYield.Api.Dex.PartialOrder (PORefs (..))

deriving stock instance Generic PORefs

deriving via CustomJSON '[FieldLabelModifier '[CamelToSnake]] PORefs instance (FromJSON PORefs)

deriving via CustomJSON '[FieldLabelModifier '[CamelToSnake]] PORefs instance (ToJSON PORefs)
