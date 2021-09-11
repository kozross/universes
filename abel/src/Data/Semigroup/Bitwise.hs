{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Semigroup.Bitwise
  ( BAnd (..),
    BIor (..),
    BXor (..),
  )
where

import Data.Bits
  ( Bits
      ( bit,
        complement,
        testBit,
        xor,
        zeroBits,
        (.&.),
        (.|.)
      ),
    FiniteBits (finiteBitSize),
  )
import Data.Kind (Type)
import Data.Monoid.CMM (CMM (monus))
import Data.Semigroup (stimes, stimesIdempotent)
import Data.Semigroup.Abelian (Abelian (factor))

-- | @since 1.0
newtype BAnd (a :: Type) = BAnd
  { -- | @since 1.0
    getBAnd :: a
  }
  deriving
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Ord,
      -- | @since 1.0
      Bits,
      -- | @since 1.0
      FiniteBits
    )
    via a
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
instance (Bits a) => Semigroup (BAnd a) where
  {-# INLINEABLE (<>) #-}
  BAnd x <> BAnd y = BAnd $ x .&. y
  {-# INLINEABLE stimes #-}
  stimes = stimesIdempotent

-- | @since 1.0
instance (FiniteBits a) => Abelian (BAnd a) where
  {-# INLINEABLE factor #-}
  BAnd x `factor` BAnd y =
    BAnd
      . getBIor
      . foldMap BIor
      <$> traverse go [0 .. finiteBitSize (undefined :: a) - 1]
    where
      go :: Int -> [a]
      go i = case (testBit x i, testBit y i) of
        (False, False) -> [bit i, zeroBits]
        (False, True) -> [zeroBits]
        (True, False) -> []
        (True, True) -> [bit i]

-- | @since 1.0
instance (FiniteBits a) => Monoid (BAnd a) where
  {-# INLINEABLE mempty #-}
  mempty = BAnd . complement $ zeroBits

-- | @since 1.0
instance (FiniteBits a) => CMM (BAnd a) where
  {-# INLINEABLE monus #-}
  BAnd x `monus` BAnd y = BAnd $ x .|. complement y

-- | @since 1.0
newtype BIor (a :: Type) = BIor
  { -- | @since 1.0
    getBIor :: a
  }
  deriving
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Ord,
      -- | @since 1.0
      Bits,
      -- | @since 1.0
      FiniteBits
    )
    via a
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
instance (Bits a) => Semigroup (BIor a) where
  {-# INLINEABLE (<>) #-}
  BIor x <> BIor y = BIor $ x .|. y
  {-# INLINEABLE stimes #-}
  stimes = stimesIdempotent

-- | @since 1.0
instance (FiniteBits a) => Abelian (BIor a) where
  {-# INLINEABLE factor #-}
  x `factor` y =
    foldMap BIor
      <$> traverse go [0 .. finiteBitSize (undefined :: a) - 1]
    where
      go :: Int -> [a]
      go i = case (testBit x i, testBit y i) of
        (False, False) -> [zeroBits]
        (True, False) -> [bit i]
        (False, True) -> []
        (True, True) -> [bit i, zeroBits]

-- | @since 1.0
instance (Bits a) => Monoid (BIor a) where
  {-# INLINEABLE mempty #-}
  mempty = BIor zeroBits

-- | @since 1.0
instance (FiniteBits a) => CMM (BIor a) where
  {-# INLINEABLE monus #-}
  BIor x `monus` BIor y = BIor $ x .&. complement y

-- | @since 1.0
newtype BXor (a :: Type) = BXor
  { -- | @since 1.0
    getBXor :: a
  }
  deriving
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Ord,
      -- | @since 1.0
      Bits,
      -- | @since 1.0
      FiniteBits
    )
    via a
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
instance (Bits a) => Semigroup (BXor a) where
  {-# INLINEABLE (<>) #-}
  BXor x <> BXor y = BXor $ x `xor` y
  {-# INLINEABLE stimes #-}
  stimes n (BXor x)
    | n <= 0 = error "stimesIdempotent: positive integral expected"
    | even n = BXor zeroBits
    | otherwise = BXor x

-- | @since 1.0
instance (Bits a) => Monoid (BXor a) where
  {-# INLINEABLE mempty #-}
  mempty = BXor zeroBits

-- | @since 1.0
instance (Bits a) => Abelian (BXor a) where
  {-# INLINEABLE factor #-}
  BXor x `factor` BXor y = BXor <$> [x `xor` y]
