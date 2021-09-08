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

deriving via (Sum Word8) instance Semigroup (NatSum Nat8)

deriving via (Sum Word16) instance Semigroup (NatSum Nat16)

deriving via (Sum Word32) instance Semigroup (NatSum Nat32)

deriving via (Sum Word64) instance Semigroup (NatSum Nat64)

deriving via (Sum Word) instance Semigroup (NatSum Nat)

deriving via (Sum Word8) instance Monoid (NatSum Nat8)

deriving via (Sum Word16) instance Monoid (NatSum Nat16)

deriving via (Sum Word32) instance Monoid (NatSum Nat32)

deriving via (Sum Word64) instance Monoid (NatSum Nat64)

deriving via (Sum Word) instance Monoid (NatSum Nat)

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

deriving via (Product Word8) instance Semigroup (NatProduct Nat8)

deriving via (Product Word16) instance Semigroup (NatProduct Nat16)

deriving via (Product Word32) instance Semigroup (NatProduct Nat32)

deriving via (Product Word64) instance Semigroup (NatProduct Nat64)

deriving via (Product Word) instance Semigroup (NatProduct Nat)

deriving via (Product Word8) instance Monoid (NatProduct Nat8)

deriving via (Product Word16) instance Monoid (NatProduct Nat16)

deriving via (Product Word32) instance Monoid (NatProduct Nat32)

deriving via (Product Word64) instance Monoid (NatProduct Nat64)

deriving via (Product Word) instance Monoid (NatProduct Nat)
