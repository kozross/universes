{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Semirig
  ( -- * Type class
    Semirig (..),

    -- * Functions
    plus,
    times,
    (+),
    (*),
    neSum,
    neProduct,
  )
where

import Data.Coerce (Coercible, coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntSet (IntSet)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup
  ( Product (Product),
    sconcat,
  )
import Data.Semigroup.Abelian (Abelian)
import Data.Semigroup.Additive (Additive (Additive))
import Data.Semigroup.Bitwise
  ( BAnd (BAnd),
    BIor (BIor),
  )
import Data.Semigroup.Intersectional (Intersectional (..))
import Data.Set (Set)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Natural (Natural)
import Prelude hiding ((*), (+))

-- | = Laws
--
-- * @x '*' (y '+' z) = (x '*' y) '+' (x '*' z) (left distributivity of '*'
--   over '+')
-- * @(x '+' y) '*' z = (x '*' z) '+' (y '*' z) (right distributivity of '*'
--   over '+')
--
-- @since 1.0
class
  ( Abelian (AddOf a),
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
instance Semirig Natural where
  type AddOf Natural = Additive Natural
  type MulOf Natural = Product Natural

-- | @since 1.0
instance Semirig Int8 where
  type AddOf Int8 = Additive Int8
  type MulOf Int8 = Product Int8

-- | @since 1.0
instance Semirig Int16 where
  type AddOf Int16 = Additive Int16
  type MulOf Int16 = Product Int16

-- | @since 1.0
instance Semirig Int32 where
  type AddOf Int32 = Additive Int32
  type MulOf Int32 = Product Int32

-- | @since 1.0
instance Semirig Int64 where
  type AddOf Int64 = Additive Int64
  type MulOf Int64 = Product Int64

-- | @since 1.0
instance Semirig Int where
  type AddOf Int = Additive Int
  type MulOf Int = Product Int

-- | @since 1.0
instance Semirig Integer where
  type AddOf Integer = Additive Integer
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
