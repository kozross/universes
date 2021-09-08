{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Semirig
  ( -- * Type class
    Semirig (..),

    -- * Helper types
    Zhegalkin (..),
    Bottleneck (..),
    Semirigged (..),

    -- * Functions
    plus,
    times,
    (+),
    (*),
    neSum,
    neProduct,
  )
where

import Data.Bits (Bits)
import Data.Coerce (Coercible, coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Nat.Internal
  ( Nat (Nat),
    Nat16 (Nat16),
    Nat32 (Nat32),
    Nat64 (Nat64),
    Nat8 (Nat8),
    NatProduct (NatProduct),
    NatSum (NatSum),
  )
import Data.Semigroup
  ( Max (Max),
    Min (Min),
    Product (Product),
    Sum (Sum),
    sconcat,
  )
import Data.Semigroup.Bitwise
  ( BAnd (BAnd),
    BIor (BIor),
    BXor (..),
  )
import Data.Semigroup.Intersectional (Intersectional (..))
import Data.Set (Set)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Natural (Natural)
import Prelude hiding ((*), (+))

-- | = Laws
--
-- * @x '+' y = y '+' x@ (commutativity of '+')
-- * @x '*' (y '+' z) = (x '*' y) '+' (x '*' z) (left distributivity of '*'
--   over '+')
-- * @(x '+' y) '*' z = (x '*' z) '+' (y '*' z) (right distributivity of '*'
--   over '+')
--
-- @since 1.0
class
  ( Semigroup (AddOf a),
    Semigroup (MulOf a),
    Coercible (AddOf a) a,
    Coercible (MulOf a) a
  ) =>
  Semirig (a :: Type)
  where
  type AddOf a :: Type
  type MulOf a :: Type

-- | @since 1.0
instance Semirig () where
  type AddOf () = ()
  type MulOf () = ()

-- | @since 1.0
instance Semirig Nat8 where
  type AddOf Nat8 = NatSum Nat8
  type MulOf Nat8 = NatProduct Nat8

-- | @since 1.0
instance Semirig Nat16 where
  type AddOf Nat16 = NatSum Nat16
  type MulOf Nat16 = NatProduct Nat16

-- | @since 1.0
instance Semirig Nat32 where
  type AddOf Nat32 = NatSum Nat32
  type MulOf Nat32 = NatProduct Nat32

-- | @since 1.0
instance Semirig Nat64 where
  type AddOf Nat64 = NatSum Nat64
  type MulOf Nat64 = NatProduct Nat64

-- | @since 1.0
instance Semirig Nat where
  type AddOf Nat = NatSum Nat
  type MulOf Nat = NatProduct Nat

-- | @since 1.0
instance Semirig Natural where
  type AddOf Natural = Sum Natural
  type MulOf Natural = Product Natural

-- | @since 1.0
instance Semirig Int8 where
  type AddOf Int8 = Sum Int8
  type MulOf Int8 = Product Int8

-- | @since 1.0
instance Semirig Int16 where
  type AddOf Int16 = Sum Int16
  type MulOf Int16 = Product Int16

-- | @since 1.0
instance Semirig Int32 where
  type AddOf Int32 = Sum Int32
  type MulOf Int32 = Product Int32

-- | @since 1.0
instance Semirig Int64 where
  type AddOf Int64 = Sum Int64
  type MulOf Int64 = Product Int64

-- | @since 1.0
instance Semirig Int where
  type AddOf Int = Sum Int
  type MulOf Int = Product Int

-- | @since 1.0
instance Semirig Integer where
  type AddOf Integer = Sum Integer
  type MulOf Integer = Product Integer

-- | @since 1.0
instance Semirig Bool where
  type AddOf Bool = BIor Bool
  type MulOf Bool = BAnd Bool

-- | @since 1.0
instance Semirig Word8 where
  type AddOf Word8 = BIor Word8
  type MulOf Word8 = BAnd Word8

-- | @since 1.0
instance Semirig Word16 where
  type AddOf Word16 = BIor Word16
  type MulOf Word16 = BAnd Word16

-- | @since 1.0
instance Semirig Word32 where
  type AddOf Word32 = BIor Word32
  type MulOf Word32 = BAnd Word32

-- | @since 1.0
instance Semirig Word64 where
  type AddOf Word64 = BIor Word64
  type MulOf Word64 = BAnd Word64

-- | @since 1.0
instance Semirig Word where
  type AddOf Word = BIor Word
  type MulOf Word = BAnd Word

-- | '+' is union, '*' is intersection.
--
-- @since 1.0
instance (Ord a) => Semirig (Set a) where
  type AddOf (Set a) = Set a
  type MulOf (Set a) = Intersectional (Set a)

-- | '+' is union, '*' is intersection.
--
-- @since 1.0
instance Semirig IntSet where
  type AddOf IntSet = IntSet
  type MulOf IntSet = Intersectional IntSet

-- | '+' is union, '*' is intersection. Different values for identical keys are
-- handled according to the value type's '<>'.
--
-- @since 1.0
instance (Ord k, Semigroup v) => Semirig (Map k v) where
  type AddOf (Map k v) = Map k v
  type MulOf (Map k v) = Intersectional (Map k v)

-- | '+' is union, '*' is intersection. Different values for identical keys are
-- handled according to the value type's '<>'.
--
-- @since 1.0
instance (Semigroup v) => Semirig (IntMap v) where
  type AddOf (IntMap v) = IntMap v
  type MulOf (IntMap v) = Intersectional (IntMap v)

-- | [Zhegalkin
-- polynomials](https://en.wikipedia.org/wiki/Zhegalkin_polynomial).
--
-- @since 1.0
newtype Zhegalkin (a :: Type) = Zhegalkin a
  deriving
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Ord
    )
    via a
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
instance (Bits a) => Semirig (Zhegalkin a) where
  type AddOf (Zhegalkin a) = BXor a
  type MulOf (Zhegalkin a) = BAnd a

-- | @since 1.0
newtype Semirigged (m :: Type -> Type) (a :: Type) = Semirigged (m a)
  deriving
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Ord,
      -- | @since 1.0
      Semigroup
    )
    via (m a)
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
instance
  (Semigroup (Intersectional (m a))) =>
  Semigroup (Intersectional (Semirigged m a))
  where
  {-# INLINEABLE (<>) #-}
  Intersectional (Semirigged sm) <> Intersectional (Semirigged sm') =
    let Intersectional intersected = Intersectional sm <> Intersectional sm'
     in Intersectional . Semirigged $ intersected

-- \'Lifts\' a 'Semirig' instance for resolution of '+' and '*' value conflicts.
--
-- @since 1.0
instance (Ord k, Semirig v) => Semirig (Semirigged (Map k) v) where
  type AddOf (Semirigged (Map k) v) = Map k (AddOf v)
  type MulOf (Semirigged (Map k) v) = Map k (MulOf v)

-- \'Lifts\' a 'Semirig' instance for resolution of '+' and '*' value conflicts.
--
-- @since 1.0
instance (Semirig v) => Semirig (Semirigged IntMap v) where
  type AddOf (Semirigged IntMap v) = IntMap (AddOf v)
  type MulOf (Semirigged IntMap v) = IntMap (MulOf v)

-- | @since 1.0
newtype Bottleneck (a :: Type) = Bottleneck a
  deriving
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Ord
    )
    via a
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
instance (Ord a) => Semirig (Bottleneck a) where
  type AddOf (Bottleneck a) = Min a
  type MulOf (Bottleneck a) = Max a

-- | Additive '<>'.
--
-- @since 1.0
{-# INLINEABLE (+) #-}
(+) :: forall (a :: Type). (Semirig a) => a -> a -> a
(+) = coerce @(AddOf a -> AddOf a -> AddOf a) (<>)

-- | Multiplicative '<>'.
--
-- @since 1.0
{-# INLINEABLE (*) #-}
(*) :: forall (a :: Type). (Semirig a) => a -> a -> a
(*) = coerce @(MulOf a -> MulOf a -> MulOf a) (<>)

-- | Synonym for @'+'@.
--
-- @since 1.0
{-# INLINEABLE plus #-}
plus :: forall (a :: Type). (Semirig a) => a -> a -> a
plus = (+)

-- | Synonym for @'*'@.
--
-- @since 1.0
{-# INLINEABLE times #-}
times :: forall (a :: Type). (Semirig a) => a -> a -> a
times = (*)

-- | @since 1.0
{-# INLINEABLE neSum #-}
neSum :: forall (a :: Type). (Semirig a) => NonEmpty a -> a
neSum = coerce @(NonEmpty (AddOf a) -> AddOf a) sconcat

-- | @since 1.0
{-# INLINEABLE neProduct #-}
neProduct :: forall (a :: Type). (Semirig a) => NonEmpty a -> a
neProduct = coerce @(NonEmpty (MulOf a) -> MulOf a) sconcat
