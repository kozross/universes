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

    -- * Helper types
    NatSum (..),
    NatProduct (..),
  )
where

import Data.Kind (Type)
import Data.Semigroup (Product (Product), Sum (Sum))
import Data.Semigroup.Abelian (Abelian (factor))
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
  type AddOf Nat8 = NatSum Nat8
  type MulOf Nat8 = NatProduct Nat8

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
  type AddOf Nat16 = NatSum Nat16
  type MulOf Nat16 = NatProduct Nat16

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
  type AddOf Nat32 = NatSum Nat32
  type MulOf Nat32 = NatProduct Nat32

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
  type AddOf Nat64 = NatSum Nat64
  type MulOf Nat64 = NatProduct Nat64

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
  type AddOf Nat = NatSum Nat
  type MulOf Nat = NatProduct Nat

-- | @since 1.0
instance Semiring Nat

-- | @since 1.0
newtype NatSum (a :: Type) = NatSum a
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
deriving via (Sum Word8) instance Semigroup (NatSum Nat8)

-- | @since 1.0
deriving via (Sum Word16) instance Semigroup (NatSum Nat16)

-- | @since 1.0
deriving via (Sum Word32) instance Semigroup (NatSum Nat32)

-- | @since 1.0
deriving via (Sum Word64) instance Semigroup (NatSum Nat64)

-- | @since 1.0
deriving via (Sum Word) instance Semigroup (NatSum Nat)

-- | @since 1.0
deriving via (Sum Word8) instance Monoid (NatSum Nat8)

-- | @since 1.0
deriving via (Sum Word16) instance Monoid (NatSum Nat16)

-- | @since 1.0
deriving via (Sum Word32) instance Monoid (NatSum Nat32)

-- | @since 1.0
deriving via (Sum Word64) instance Monoid (NatSum Nat64)

-- | @since 1.0
deriving via (Sum Word) instance Monoid (NatSum Nat)

-- | @since 1.0
instance Abelian (NatSum Nat8) where
  {-# INLINEABLE factor #-}
  NatSum (Nat8 x) `factor` NatSum (Nat8 y) =
    NatSum . Nat8 <$> [x - y]

-- | @since 1.0
instance Abelian (NatSum Nat16) where
  {-# INLINEABLE factor #-}
  NatSum (Nat16 x) `factor` NatSum (Nat16 y) =
    NatSum . Nat16 <$> [x - y]

-- | @since 1.0
instance Abelian (NatSum Nat32) where
  {-# INLINEABLE factor #-}
  NatSum (Nat32 x) `factor` NatSum (Nat32 y) =
    NatSum . Nat32 <$> [x - y]

-- | @since 1.0
instance Abelian (NatSum Nat64) where
  {-# INLINEABLE factor #-}
  NatSum (Nat64 x) `factor` NatSum (Nat64 y) =
    NatSum . Nat64 <$> [x - y]

-- | @since 1.0
instance Abelian (NatSum Nat) where
  {-# INLINEABLE factor #-}
  NatSum (Nat x) `factor` NatSum (Nat y) =
    NatSum . Nat <$> [x - y]

-- | @since 1.0
newtype NatProduct (a :: Type) = NatProduct a
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
deriving via (Product Word8) instance Semigroup (NatProduct Nat8)

-- | @since 1.0
deriving via (Product Word16) instance Semigroup (NatProduct Nat16)

-- | @since 1.0
deriving via (Product Word32) instance Semigroup (NatProduct Nat32)

-- | @since 1.0
deriving via (Product Word64) instance Semigroup (NatProduct Nat64)

-- | @since 1.0
deriving via (Product Word) instance Semigroup (NatProduct Nat)

-- | @since 1.0
deriving via (Product Word8) instance Monoid (NatProduct Nat8)

-- | @since 1.0
deriving via (Product Word16) instance Monoid (NatProduct Nat16)

-- | @since 1.0
deriving via (Product Word32) instance Monoid (NatProduct Nat32)

-- | @since 1.0
deriving via (Product Word64) instance Monoid (NatProduct Nat64)

-- | @since 1.0
deriving via (Product Word) instance Monoid (NatProduct Nat)
