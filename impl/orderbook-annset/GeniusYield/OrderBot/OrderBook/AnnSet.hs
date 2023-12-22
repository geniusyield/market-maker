{-# LANGUAGE RecordWildCards #-}

module GeniusYield.OrderBot.OrderBook.AnnSet (
  -- * Core Order book types
  MultiAssetOrderBook,
  mkMultiAssetOrderBook,
  maOrderBookToList,
  OrderBook (..),

  -- * Order book components
  Orders (..),
  LiquidityPositions,
  liquidityPositions,

  -- * Order book construction
  populateOrderBook,
  buildOrderBookList,
  -- * Order book queries
  lowestSell,
  highestBuy,
  withoutTip,
  foldlOrders,
  foldrOrders,
  ordersLTPrice,
  ordersLTEPrice,
  ordersGTPrice,
  ordersGTEPrice,
  volumeLTPrice,
  volumeLTEPrice,
  volumeGTPrice,
  volumeGTEPrice,

  -- * MultiAssetOrderBook reading utilities
  withEachAsset,
) where

import           Data.Aeson                       (ToJSON, (.=))
import qualified Data.Aeson                       as Aeson
import qualified Data.Aeson.Types                 as Aeson
import           Data.Foldable                    (foldl')
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as M

import           GeniusYield.OrderBot.DataSource  (Connection,
                                                   withEachAssetOrders)
import           GeniusYield.OrderBot.Types
import           GeniusYield.Types                (showTxOutRef)
import           GeniusYield.Api.Dex.Types        ( HasDexScripts )
import           GeniusYield.Api.Dex.PartialOrder ( PORefs )

import qualified GeniusYield.AnnSet.Internal     as AnnSet

type MultiAssetOrderBook = Map OrderAssetPair OrderBook

mkMultiAssetOrderBook :: [(OrderAssetPair, OrderBook)] -> MultiAssetOrderBook
mkMultiAssetOrderBook = M.fromList

maOrderBookToList :: MultiAssetOrderBook -> [(OrderAssetPair, OrderBook)]
maOrderBookToList = M.toList

{- Why can't this be a type synonym?

In general, types declared as 'data' in the signature can be type synonyms in the actual impl
_only if they are not parameterized_. Since 'Orders' is declared as a parameterized 'data' type
in the signature - it cannot be a type synonym in the implementation.

The reasoning is of course obvious: the signature expects a type that satisfied the generativity
axiom, i.e a type that can be partially applied. However, type synonyms violate this rule - and thus,
an implementation may not use a type synonym in place of a parameterized data type.
-}
newtype Orders t = Orders {unOrders :: AnnSet.Orders t}
  deriving newtype (Eq, Show)

data OrderBook = OrderBook
  { sellOrders :: Orders 'SellOrder
  , buyOrders  :: Orders 'BuyOrder
  }
  deriving stock (Show, Eq)

instance ToJSON OrderBook where
    toJSON OrderBook{sellOrders, buyOrders} =
        Aeson.object
        [ "sellOrders" .= sellOrders
        , "buyOrders" .= buyOrders
        ]

instance ToJSON (Orders t) where
    toJSON = Aeson.listValue sOrderToValue . AnnSet.toAscList . unOrders
      where
        sOrderToValue :: AnnSet.OrderView t -> Aeson.Value
        sOrderToValue AnnSet.OrderView{orderInfo} =
            let v = volume orderInfo in
                Aeson.object
                [ "orderRef" .= showTxOutRef (orderRef orderInfo)
                , "orderType" .= show (orderType orderInfo)
                , "assetInfo" .= assetInfo orderInfo
                , "volume" .= Aeson.object [ "min" .= volumeMin v
                                           , "max" .= volumeMax v
                                           ]
                , "price" .= show (getPrice $ price orderInfo)
                ]

type LiquidityPositions = ()

liquidityPositions :: OrderBook -> LiquidityPositions
liquidityPositions _ = ()

populateOrderBook
    :: HasDexScripts a
    => Connection
    -> a
    -> PORefs
    -> [OrderAssetPair]
    -> IO MultiAssetOrderBook
populateOrderBook conn dex poRefs f = do
  multiAssetBookL <-
    withEachAssetOrders
      conn
      dex
      poRefs
      f
      buildOrderBookList
      []
  pure $ mkMultiAssetOrderBook multiAssetBookL

buildOrderBookList
  :: [(OrderAssetPair, OrderBook)]
  -> (# OrderAssetPair, [OrderInfo 'BuyOrder], [OrderInfo 'SellOrder] #)
  -> [(OrderAssetPair, OrderBook)]
buildOrderBookList acc (# _, [], [] #) = acc
buildOrderBookList acc (# oap, buyOrders, [] #) = (oap, OrderBook (Orders AnnSet.Empty) (Orders $ AnnSet.mkOrders buyOrders)) : acc
buildOrderBookList acc (# oap, [], sellOrders #) = (oap, OrderBook (Orders $ AnnSet.mkOrders sellOrders) (Orders AnnSet.Empty)) : acc
buildOrderBookList acc (# oap, buyOrders, sellOrders #) =
  let buyOrders' = Orders $ AnnSet.mkOrders buyOrders
      sellOrders' = Orders $ AnnSet.mkOrders sellOrders in
   (oap, OrderBook sellOrders' buyOrders') : acc

lowestSell :: Orders 'SellOrder -> OrderInfo 'SellOrder
lowestSell = AnnSet.orderInfo . AnnSet.findMin . unOrders

highestBuy :: Orders 'BuyOrder -> OrderInfo 'BuyOrder
highestBuy = AnnSet.orderInfo . AnnSet.findMin . unOrders

withoutTip :: Orders t -> Orders t
withoutTip (Orders AnnSet.Empty)             = Orders AnnSet.Empty
withoutTip (Orders (AnnSet.Entry _ _ _ l r)) = Orders (AnnSet.glue l r)

foldlOrders :: forall a t. (a -> OrderInfo t -> a) -> a -> Orders t -> a
foldlOrders f e (Orders os) = foldl' g e os
  where
    g :: (a -> AnnSet.OrderView t -> a)
    g a AnnSet.OrderView {..} = f a orderInfo

foldrOrders :: forall a t. (OrderInfo t -> a -> a) -> a -> Orders t -> a
foldrOrders f e (Orders os) = foldr (f . AnnSet.orderInfo) e os

ordersLTPrice :: Price -> Orders t -> Orders t
ordersLTPrice maxPrice = Orders . AnnSet.takeWhileAntitone ((< maxPrice) . AnnSet.orderPrice) . unOrders

ordersLTEPrice :: Price -> Orders t -> Orders t
ordersLTEPrice maxPrice = Orders . AnnSet.takeWhileAntitone ((<= maxPrice) . AnnSet.orderPrice) . unOrders

ordersGTPrice :: Price -> Orders t -> Orders t
ordersGTPrice minPrice = Orders . AnnSet.takeWhileAntitone ((> minPrice) . AnnSet.orderPrice) . unOrders

ordersGTEPrice :: Price -> Orders t -> Orders t
ordersGTEPrice minPrice = Orders . AnnSet.takeWhileAntitone ((>= minPrice) . AnnSet.orderPrice) . unOrders

volumeLTPrice :: Price -> Orders t -> Volume
volumeLTPrice maxPrice = AnnSet.totalVolume . AnnSet.measure . unOrders . ordersLTPrice maxPrice

volumeLTEPrice :: Price -> Orders t -> Volume
volumeLTEPrice maxPrice = AnnSet.totalVolume . AnnSet.measure . unOrders . ordersLTEPrice maxPrice

volumeGTPrice :: Price -> Orders t -> Volume
volumeGTPrice minPrice = AnnSet.totalVolume . AnnSet.measure . unOrders . ordersGTPrice minPrice

volumeGTEPrice :: Price -> Orders t -> Volume
volumeGTEPrice minPrice = AnnSet.totalVolume . AnnSet.measure . unOrders . ordersGTEPrice minPrice

withEachAsset :: (OrderAssetPair -> OrderBook -> [a]) -> MultiAssetOrderBook -> [a]
withEachAsset f = M.foldrWithKey (\p b acc -> f p b ++ acc) mempty
