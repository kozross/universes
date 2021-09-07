{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module Data.Semigroup.LCM (LCM (..)) where

import Data.Kind (Type)
import Data.Semigroup (stimes, stimesIdempotent)
import Numeric.Natural (Natural)

-- | @since 1.0
newtype LCM (a :: Type) = LCM a
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
instance Semigroup (LCM Natural) where
  {-# INLINEABLE (<>) #-}
  LCM n <> LCM m = LCM . lcm n $ m
  {-# INLINEABLE stimes #-}
  stimes = stimesIdempotent

-- | @since 1.0
instance Monoid (LCM Natural) where
  {-# INLINEABLE mempty #-}
  mempty = LCM 1

-- | @since 1.0
instance Semigroup (LCM Integer) where
  {-# INLINEABLE (<>) #-}
  LCM n <> LCM m = LCM . lcm n $ m
  {-# INLINEABLE stimes #-}
  stimes n (LCM x)
    | n <= 0 = error "stimes: positive integral expected."
    | otherwise = LCM . abs $ x
