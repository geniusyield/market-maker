module GeniusYield.MarketMaker.Prices.Taptools where

import           Control.Exception       (Exception (..), displayException,
                                          fromException, toException)
import           Data.Aeson
import qualified Data.Aeson              as Aeson
import qualified Data.Aeson.Types        as Aeson
import           Data.Bifunctor          (Bifunctor (bimap))
import qualified Data.Map.Strict         as Map
import           Data.Proxy
import           Data.Text               (Text, unpack)
import qualified Data.Text               as Text
import           Data.Text.Encoding      (encodeUtf8)
import           GeniusYield.GYConfig    (Confidential (..))
import           GeniusYield.Types       (GYAssetClass, makeAssetClass)
import           Network.HTTP.Client     (HttpException (..), Manager,
                                          ManagerSettings (..), Request (..),
                                          newManager, requestHeaders)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API
import           Servant.Client
import qualified Servant.Client.Core     as Servant

newtype TtUnit = TtUnit {unTtUnit :: GYAssetClass}
  deriving stock (Eq, Ord, Show)

instance ToHttpApiData TtUnit where
  toUrlPiece (TtUnit ac) = removeDot $ toUrlPiece ac
    where
      removeDot = Text.filter (/= '.')

instance Aeson.ToJSON TtUnit where
  toJSON = Aeson.toJSON . toUrlPiece

instance Aeson.ToJSONKey TtUnit where
    toJSONKey = Aeson.toJSONKeyText toUrlPiece

instance FromHttpApiData TtUnit where
  parseUrlPiece t =
    let (pid, tn) = Text.splitAt 56 t
    in bimap Text.pack TtUnit $ makeAssetClass pid tn

instance Aeson.FromJSON TtUnit where
  parseJSON = Aeson.withText "TtUnit" $ \t -> case parseUrlPiece t of
    Left e    -> fail $ show e
    Right ttu -> pure ttu

instance Aeson.FromJSONKey TtUnit where
    fromJSONKey = Aeson.FromJSONKeyTextParser (either (fail . show) pure . parseUrlPiece)

type PricesResponse = Map.Map TtUnit Double

type TtAPI =
  "token" :> "prices" :> ReqBody '[JSON] [TtUnit] :> Post '[JSON] PricesResponse

data TaptoolsPriceException
  = TaptoolsError !Text               -- ^ Other taptools error.
  | TaptoolsClientError !ClientError  -- ^ Servant client error while querying taptools.
  deriving stock (Eq, Show)

instance Exception TaptoolsPriceException where
  displayException (TaptoolsError msg) = "Taptools Error: " ++ unpack msg
  displayException (TaptoolsClientError err) = "Taptools Client Error: " ++ displayException (sanitizeClientError err)

sanitizeClientError :: ClientError -> ClientError
sanitizeClientError err = case err of
  FailureResponse reqF res -> FailureResponse reqF { Servant.requestHeaders = hideHeader <$> Servant.requestHeaders reqF } res
  ConnectionError se       -> ConnectionError $ case fromException se of
    Just (HttpExceptionRequest req content) ->
      let req' = req { requestHeaders = map hideHeader (requestHeaders req) }
      in  toException (HttpExceptionRequest req' content)
    _anyOther                               -> se
  _anyOther                -> err
  where
    hideHeader (h, v) = if h == "x-api-key" then (h, "hidden") else (h, v)

api :: Proxy TtAPI
api = Proxy

getTtPrice :: [TtUnit] -> ClientM PricesResponse
getTtPrice = client api

taptoolsManager :: Confidential Text -> IO Manager
taptoolsManager apiKey = newManager $ tlsManagerSettings { managerModifyRequest = withHeaders }
  where
    Confidential apiKey' = apiKey

    withHeaders :: Request -> IO Request
    withHeaders req = do
      return $ req { requestHeaders = ("x-api-key", encodeUtf8 apiKey') :
                                      filter (("x-api-key" /=) . fst) (requestHeaders req)
                   }

taptoolsEnv :: Confidential Text -> IO ClientEnv
taptoolsEnv apiKey = do
  manager' <- taptoolsManager apiKey
  return $ mkClientEnv manager' taptoolsBaseUrl

taptoolsBaseUrl :: BaseUrl
taptoolsBaseUrl = BaseUrl Https "openapi.taptools.io" 443 "api/v1"

priceFromTaptools :: TtUnit -> ClientEnv -> IO (Either ClientError PricesResponse)
priceFromTaptools unit = runClientM (getTtPrice [unit])
