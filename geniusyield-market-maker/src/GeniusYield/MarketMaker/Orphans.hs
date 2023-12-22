{-# OPTIONS_GHC -Wno-orphans #-}

module GeniusYield.MarketMaker.Orphans where

import Deriving.Aeson
import GeniusYield.Api.Dex.PartialOrder (PORefs (..))
import GeniusYield.GYConfig (Confidential (..))

deriving stock instance Generic PORefs

deriving via CustomJSON '[FieldLabelModifier '[CamelToSnake]] PORefs instance (FromJSON PORefs)

deriving via CustomJSON '[FieldLabelModifier '[CamelToSnake]] PORefs instance (ToJSON PORefs)

deriving newtype instance ToJSON a ⇒ ToJSON (Confidential a)
