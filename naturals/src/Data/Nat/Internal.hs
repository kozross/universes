{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Nat.Internal
  ( -- * Base types
    Nat8 (..),
    Nat16 (..),
    Nat32 (..),
    Nat64 (..),
    Nat (..),
  )
where

import Data.Semigroup (Product (Product), Sum (Sum))
import Data.Semigroup.Abelian (Abelian (factor))
import Data.Semigroup.Additive (Additive (Additive))
import Data.Semigroup.Multiplicative (Multiplicative (Multiplicative))
import Data.Semirig (Semirig (AddOf, MulOf))
import Data.Semiring (Semiring (fromNatural))
import Data.Word (Word16, Word32, Word64, Word8)

-- | @since 1.0
newtype Nat8 = Nat8 Word8
  deriving
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Ord
    )
    via Word8
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
instance Semirig Nat8 where
  type AddOf Nat8 = Additive Nat8
  type MulOf Nat8 = Multiplicative Nat8

-- | @since 1.0
instance Semiring Nat8 where
  {-# INLINEABLE fromNatural #-}
  fromNatural n = Nat8 . fromIntegral $ n `rem` 256

-- | @since 1.0
newtype Nat16 = Nat16 Word16
  deriving
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Ord
    )
    via Word16
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
instance Semirig Nat16 where
  type AddOf Nat16 = Additive Nat16
  type MulOf Nat16 = Multiplicative Nat16

-- | @since 1.0
instance Semiring Nat16 where
  {-# INLINEABLE fromNatural #-}
  fromNatural n = Nat16 . fromIntegral $ n `rem` 65536

-- | @since 1.0
newtype Nat32 = Nat32 Word32
  deriving
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Ord
    )
    via Word32
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
instance Semirig Nat32 where
  type AddOf Nat32 = Additive Nat32
  type MulOf Nat32 = Multiplicative Nat32

-- | @since 1.0
instance Semiring Nat32 where
  {-# INLINEABLE fromNatural #-}
  fromNatural n = Nat32 . fromIntegral $ n `rem` 4294967296

-- | @since 1.0
newtype Nat64 = Nat64 Word64
  deriving
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Ord
    )
    via Word64
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
instance Semirig Nat64 where
  type AddOf Nat64 = Additive Nat64
  type MulOf Nat64 = Multiplicative Nat64

-- | @since 1.0
instance Semiring Nat64 where
  {-# INLINEABLE fromNatural #-}
  fromNatural n = Nat64 . fromIntegral $ n `rem` 18446744073709551616

-- | @since 1.0
newtype Nat = Nat Word
  deriving
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Ord
    )
    via Word
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
instance Semirig Nat where
  type AddOf Nat = Additive Nat
  type MulOf Nat = Multiplicative Nat

-- | @since 1.0
instance Semiring Nat

-- | @since 1.0
deriving via (Sum Word8) instance Semigroup (Additive Nat8)

-- | @since 1.0
deriving via (Sum Word16) instance Semigroup (Additive Nat16)

-- | @since 1.0
deriving via (Sum Word32) instance Semigroup (Additive Nat32)

-- | @since 1.0
deriving via (Sum Word64) instance Semigroup (Additive Nat64)

-- | @since 1.0
deriving via (Sum Word) instance Semigroup (Additive Nat)

-- | @since 1.0
deriving via (Sum Word8) instance Monoid (Additive Nat8)

-- | @since 1.0
deriving via (Sum Word16) instance Monoid (Additive Nat16)

-- | @since 1.0
deriving via (Sum Word32) instance Monoid (Additive Nat32)

-- | @since 1.0
deriving via (Sum Word64) instance Monoid (Additive Nat64)

-- | @since 1.0
deriving via (Sum Word) instance Monoid (Additive Nat)

-- | @since 1.0
instance Abelian (Additive Nat8) where
  {-# INLINEABLE factor #-}
  Additive (Nat8 x) `factor` Additive (Nat8 y) =
    Additive . Nat8 <$> [x - y]

-- | @since 1.0
instance Abelian (Additive Nat16) where
  {-# INLINEABLE factor #-}
  Additive (Nat16 x) `factor` Additive (Nat16 y) =
    Additive . Nat16 <$> [x - y]

-- | @since 1.0
instance Abelian (Additive Nat32) where
  {-# INLINEABLE factor #-}
  Additive (Nat32 x) `factor` Additive (Nat32 y) =
    Additive . Nat32 <$> [x - y]

-- | @since 1.0
instance Abelian (Additive Nat64) where
  {-# INLINEABLE factor #-}
  Additive (Nat64 x) `factor` Additive (Nat64 y) =
    Additive . Nat64 <$> [x - y]

-- | @since 1.0
instance Abelian (Additive Nat) where
  {-# INLINEABLE factor #-}
  Additive (Nat x) `factor` Additive (Nat y) =
    Additive . Nat <$> [x - y]

-- | @since 1.0
deriving via (Product Word8) instance Semigroup (Multiplicative Nat8)

-- | @since 1.0
deriving via (Product Word16) instance Semigroup (Multiplicative Nat16)

-- | @since 1.0
deriving via (Product Word32) instance Semigroup (Multiplicative Nat32)

-- | @since 1.0
deriving via (Product Word64) instance Semigroup (Multiplicative Nat64)

-- | @since 1.0
deriving via (Product Word) instance Semigroup (Multiplicative Nat)

-- | @since 1.0
deriving via (Product Word8) instance Monoid (Multiplicative Nat8)

-- | @since 1.0
deriving via (Product Word16) instance Monoid (Multiplicative Nat16)

-- | @since 1.0
deriving via (Product Word32) instance Monoid (Multiplicative Nat32)

-- | @since 1.0
deriving via (Product Word64) instance Monoid (Multiplicative Nat64)

-- | @since 1.0
deriving via (Product Word) instance Monoid (Multiplicative Nat)
