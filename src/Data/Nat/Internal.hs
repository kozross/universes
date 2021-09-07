{-# LANGUAGE DerivingVia #-}
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
import Data.Semirig (Semirig (AddOf, MulOf))
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
  type AddOf Nat8 = Sum Word8
  type MulOf Nat8 = Product Word8

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
  type AddOf Nat16 = Sum Word16
  type MulOf Nat16 = Product Word16

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
  type AddOf Nat32 = Sum Word32
  type MulOf Nat32 = Product Word32

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
  type AddOf Nat64 = Sum Word64
  type MulOf Nat64 = Product Word64

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
  type AddOf Nat = Sum Word
  type MulOf Nat = Product Word
