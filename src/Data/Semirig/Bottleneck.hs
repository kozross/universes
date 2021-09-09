{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Semirig.Bottleneck
  ( Bottleneck (..),
  )
where

import Data.Kind (Type)
import Data.Semigroup (Max (Max), Min (Min))
import Data.Semirig (Semirig (AddOf, MulOf))

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
