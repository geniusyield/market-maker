{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

{- | This module contains the basic helper types and classes used to implement the order matching book.

     The Book data structure itself can be found in the
-}
module GeniusYield.AnnSet.Types where

import Data.Kind (Type)
import Data.Ord
import GHC.Natural (Natural)
import GeniusYield.OrderBot.Types
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances.Natural ()

{- | We might have called this "time". Orders should be ranked in ascending order of
   their submission time, such that (e.g.) the first chronological order has a priority of 0, the second
   has a priority of 1, and so on. If two orders are equivalent except with respect to their chronological order,
   the older order (with the lower priority) should be filled first.
-}
newtype Priority = Priority {getPriority :: Natural} deriving (Show, Eq, Ord, Enum)

instance Arbitrary Priority where
  arbitrary = Priority <$> arbitrary

{- | A class for things that can be measured. Concretely, this is used to add a volume annotation to
   the Book structure so that the total volume contained in a given Book can always be retrieved
   in O(1) time. Because the Book(s) will be read-only, after a Book is constructed the volume for any
   sub-Book (Book is quite like Set from Data.Set) never needs to be recalculated.
-}
class Monoid (MeasureOut a) => Measured a where
  type MeasureOut a :: Type
  measure :: a -> MeasureOut a

instance Measured Natural where
  type MeasureOut Natural = Volume
  measure n = Volume n n

instance Measured a => Measured (Either a a) where
  type MeasureOut (Either a a) = MeasureOut a
  measure = \case
    Left x -> measure x
    Right x -> measure x

{-# SPECIALIZE measure :: OrderView -> Volume #-}

-- | A type which represents the "sortedness" of an order book. See the two ord instances for OrderView below.
data Bias = Min | Max deriving (Show, Eq)

-- | A singleton for Bias. Provides some additional type safety.
data SBias :: Bias -> Type where
  SMin :: SBias 'Min
  SMax :: SBias 'Max

deriving instance Show (SBias b)

-- | Ad-hoc class for implicit singletons of Bias. Not worth importing Data.Singletons for this.
class Known (b :: Bias) where
  sing :: SBias b

instance Known 'Min where
  sing = SMin

instance Known 'Max where
  sing = SMax

-- | A summary view of an order.
data OrderView t = OrderView
  { orderInfo :: !(OrderInfo t)
  , orderPriority :: !Priority
  , bias :: !(SBias (OrderTypeToBias t))
  }

deriving instance Show (OrderView a)

type OrderTypeToBias :: OrderType -> Bias
type family OrderTypeToBias t = b | b -> t where
  OrderTypeToBias 'BuyOrder = 'Max
  OrderTypeToBias 'SellOrder = 'Min

orderPrice :: OrderView t -> Price
orderPrice = price . orderInfo

orderVolume :: OrderView t -> Volume
orderVolume = volume . orderInfo

{-
instance Measured Volume (OrderView b a) where
  measure = orderVolume
  {-# INLINE measure #-}
-}
instance Eq (OrderView t) where
  (OrderView x t1 _) == (OrderView y t2 _) = t1 == t2 && x == y

instance Ord (OrderView 'SellOrder) where
  compare ov1 ov2 = case compare (orderPrice ov1) (orderPrice ov2) of
    EQ -> case compare (orderVolume ov1) (orderVolume ov2) of
      EQ -> compare (Down $ orderPriority ov1) (Down $ orderPriority ov2)
      other -> other
    other -> other

instance Ord (OrderView 'BuyOrder) where
  compare ov1 ov2 = case compare (Down $ orderPrice ov1) (Down $ orderPrice ov2) of
    EQ -> case compare (orderVolume ov1) (orderVolume ov2) of
      EQ -> compare (Down $ orderPriority ov1) (Down $ orderPriority ov2)
      other -> other
    other -> other

type PriceRange = (Price, Price)

data Priced :: Bias -> Type where
  MkPriced :: SBias b -> Price -> AnyOrderInfo -> Priced b
  NoPrice :: Priced b

instance Semigroup (Priced 'Min) where
  px@(MkPriced _ p1 _) <> py@(MkPriced _ p2 _)
    | p1 < p2 = px
    | otherwise = py
  NoPrice <> b = b
  b <> NoPrice = b

instance Monoid (Priced 'Min) where
  mempty = NoPrice

instance Semigroup (Priced 'Max) where
  px@(MkPriced _ p1 _) <> py@(MkPriced _ p2 _)
    | p1 > p2 = px
    | otherwise = py
  NoPrice <> b = b
  b <> NoPrice = b

instance Monoid (Priced 'Max) where
  mempty = NoPrice

type MinPriced = Priced 'Min

minPriced :: forall a. Price -> OrderInfo a -> Priced 'Min
minPriced p = MkPriced SMin p . AnyOrderInfo

type MaxPriced = Priced 'Max

maxPriced :: Price -> OrderInfo a -> Priced 'Max
maxPriced p = MkPriced SMax p . AnyOrderInfo

unPriced :: Priced b -> Price
unPriced NoPrice = Price 0
unPriced (MkPriced _ p _) = p

data AnyOrderInfo = forall t. AnyOrderInfo (OrderInfo t)

asBuyOrder :: AnyOrderInfo -> Maybe (OrderInfo 'BuyOrder)
asBuyOrder (AnyOrderInfo x@OrderInfo {orderType = SBuyOrder}) = Just x
asBuyOrder _ = Nothing

asSellOrder :: AnyOrderInfo -> Maybe (OrderInfo 'SellOrder)
asSellOrder (AnyOrderInfo x@OrderInfo {orderType = SSellOrder}) = Just x
asSellOrder _ = Nothing

data Volumed :: Bias -> Type where
  MkVolumed :: SBias b -> Natural -> AnyOrderInfo -> Volumed b
  NoVolume :: Volumed b

instance Semigroup (Volumed 'Min) where
  vx@(MkVolumed _ v1 _) <> vy@(MkVolumed _ v2 _)
    | v1 < v2 = vx
    | otherwise = vy
  vx <> NoVolume = vx
  NoVolume <> vx = vx

instance Monoid (Volumed 'Min) where
  mempty = NoVolume

instance Semigroup (Volumed 'Max) where
  vx@(MkVolumed _ v1 _) <> vy@(MkVolumed _ v2 _)
    | v1 > v2 = vx
    | otherwise = vy
  vx <> NoVolume = vx
  NoVolume <> vx = vx

instance Monoid (Volumed 'Max) where
  mempty = NoVolume

type MinVolumed = Volumed 'Min

minVolumed :: Natural -> OrderInfo a -> Volumed 'Min
minVolumed p = MkVolumed SMin p . AnyOrderInfo

maxVolumed :: Natural -> OrderInfo a -> Volumed 'Max
maxVolumed p = MkVolumed SMax p . AnyOrderInfo

unVolumed :: Volumed b -> Natural
unVolumed NoVolume = 0
unVolumed (MkVolumed _ v _) = v

type MaxVolumed = Volumed 'Max

data Summary = Summary
  { totalVolume :: Volume
  , lowestVolume :: MinVolumed
  , highestVolume :: MaxVolumed
  , totalValue :: PriceRange
  , highestPrice :: MaxPriced
  , lowestPrice :: MinPriced
  }

instance Semigroup Summary where
  (Summary tvo lv hv tva hp lp) <> (Summary tvo' lv' hv' tva' hp' lp') =
    Summary
      (tvo <> tvo')
      (lv <> lv')
      (hv <> hv')
      (tva <> tva')
      (hp <> hp')
      (lp <> lp')

instance Monoid Summary where
  mempty = Summary mempty mempty mempty mempty mempty mempty

instance Measured (OrderView a) where
  type MeasureOut (OrderView a) = Summary
  measure OrderView {..} =
    Summary
      { totalVolume = Volume lvo' hvo'
      , lowestVolume = lvo
      , highestVolume = hvo
      , totalValue = tva
      , highestPrice = hva
      , lowestPrice = lva
      }
    where
      Volume !lvo' !hvo' = volume orderInfo
      Price op = price orderInfo
      !lva' = Price $ toRational lvo' * op
      !hva' = Price $ toRational hvo' * op
      !lva = minPriced lva' orderInfo
      !hva = maxPriced hva' orderInfo
      !tva = (lva', hva')
      !lvo = minVolumed lvo' orderInfo
      !hvo = maxVolumed hvo' orderInfo
