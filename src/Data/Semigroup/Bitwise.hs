{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}

module Data.Semigroup.Bitwise
  ( BAnd (..),
    BIor (..),
    BXor (..),
  )
where

import Data.Bits
  ( Bits (xor, zeroBits, (.&.), (.|.)),
    FiniteBits,
  )
import Data.Kind (Type)
import Data.Semigroup (stimes, stimesIdempotent)

-- | @since 1.0
newtype BAnd (a :: Type) = BAnd a
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

instance (Bits a) => Semigroup (BAnd a) where
  {-# INLINEABLE (<>) #-}
  BAnd x <> BAnd y = BAnd $ x .&. y
  {-# INLINEABLE stimes #-}
  stimes = stimesIdempotent

-- | @since 1.0
newtype BIor (a :: Type) = BIor a
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

instance (Bits a) => Semigroup (BIor a) where
  {-# INLINEABLE (<>) #-}
  BIor x <> BIor y = BIor $ x .|. y
  {-# INLINEABLE stimes #-}
  stimes = stimesIdempotent

-- | @since 1.0
newtype BXor (a :: Type) = BXor a
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

instance (Bits a) => Semigroup (BXor a) where
  {-# INLINEABLE (<>) #-}
  BXor x <> BXor y = BXor $ x `xor` y
  {-# INLINEABLE stimes #-}
  stimes n (BXor x)
    | n <= 0 = error "stimesIdempotent: positive integral expected"
    | even n = BXor zeroBits
    | otherwise = BXor x
