module GeniusYield.MarketMaker.Logging (

) where

import           Control.Monad.IO.Class (MonadIO (..))
import           GeniusYield.Types
import           GHC.Stack              (HasCallStack, withFrozenCallStack)

gyLogInfoWithAction :: (HasCallStack, MonadIO m) => (GYLogNamespace -> GYLogSeverity -> String -> IO ()) -> GYProviders -> GYLogNamespace -> String -> m ()
gyLogInfoWithAction logAction providers ns msg = withFrozenCallStack $ do
  liftIO $ logAction ns GYInfo msg
  gyLogInfo providers ns msg
