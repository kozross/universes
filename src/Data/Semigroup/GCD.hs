{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module Data.Semigroup.GCD (GCD (..)) where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Nat.Internal
  ( Nat (Nat),
    Nat16 (Nat16),
    Nat32 (Nat32),
    Nat64 (Nat64),
    Nat8 (Nat8),
  )
import Data.Semigroup (stimes, stimesIdempotent)
import Numeric.Natural (Natural)

-- | @since 1.0
newtype GCD (a :: Type) = GCD a
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

instance Semigroup (GCD Natural) where
  {-# INLINEABLE (<>) #-}
  GCD n <> GCD m = GCD . gcd n $ m
  {-# INLINEABLE stimes #-}
  stimes = stimesIdempotent

instance Semigroup (GCD Nat8) where
  {-# INLINEABLE (<>) #-}
  GCD (Nat8 n) <> GCD (Nat8 m) =
    GCD . Nat8 . gcd n $ m
  {-# INLINEABLE stimes #-}
  stimes = stimesIdempotent

instance Semigroup (GCD Nat16) where
  {-# INLINEABLE (<>) #-}
  GCD (Nat16 n) <> GCD (Nat16 m) =
    GCD . Nat16 . gcd n $ m
  {-# INLINEABLE stimes #-}
  stimes = stimesIdempotent

instance Semigroup (GCD Nat32) where
  {-# INLINEABLE (<>) #-}
  GCD (Nat32 n) <> GCD (Nat32 m) =
    GCD . Nat32 . gcd n $ m
  {-# INLINEABLE stimes #-}
  stimes = stimesIdempotent

instance Semigroup (GCD Nat64) where
  {-# INLINEABLE (<>) #-}
  GCD (Nat64 n) <> GCD (Nat64 m) =
    GCD . Nat64 . gcd n $ m
  {-# INLINEABLE stimes #-}
  stimes = stimesIdempotent

instance Semigroup (GCD Nat) where
  {-# INLINEABLE (<>) #-}
  GCD (Nat n) <> GCD (Nat m) =
    GCD . Nat . gcd n $ m
  {-# INLINEABLE stimes #-}
  stimes = stimesIdempotent

instance Semigroup (GCD Integer) where
  {-# INLINEABLE (<>) #-}
  GCD n <> GCD m = GCD . gcd n $ m
  {-# INLINEABLE stimes #-}
  stimes n (GCD x)
    | n <= 0 = error "stimes: positive integral expected."
    | otherwise = GCD . abs $ x

instance Semigroup (GCD Int8) where
  {-# INLINEABLE (<>) #-}
  GCD n <> GCD m = GCD . gcd n $ m
  {-# INLINEABLE stimes #-}
  stimes n (GCD x)
    | n <= 0 = error "stimes: positive integral expected."
    | otherwise = GCD . abs $ x

instance Semigroup (GCD Int16) where
  {-# INLINEABLE (<>) #-}
  GCD n <> GCD m = GCD . gcd n $ m
  {-# INLINEABLE stimes #-}
  stimes n (GCD x)
    | n <= 0 = error "stimes: positive integral expected."
    | otherwise = GCD . abs $ x

instance Semigroup (GCD Int32) where
  {-# INLINEABLE (<>) #-}
  GCD n <> GCD m = GCD . gcd n $ m
  {-# INLINEABLE stimes #-}
  stimes n (GCD x)
    | n <= 0 = error "stimes: positive integral expected."
    | otherwise = GCD . abs $ x

instance Semigroup (GCD Int64) where
  {-# INLINEABLE (<>) #-}
  GCD n <> GCD m = GCD . gcd n $ m
  {-# INLINEABLE stimes #-}
  stimes n (GCD x)
    | n <= 0 = error "stimes: positive integral expected."
    | otherwise = GCD . abs $ x

instance Semigroup (GCD Int) where
  {-# INLINEABLE (<>) #-}
  GCD n <> GCD m = GCD . gcd n $ m
  {-# INLINEABLE stimes #-}
  stimes n (GCD x)
    | n <= 0 = error "stimes: positive integral expected."
    | otherwise = GCD . abs $ x
