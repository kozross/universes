{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module Data.Semigroup.Additive (Additive (..)) where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Semigroup (stimes)
import Data.Semigroup.Abelian (Abelian (factor))
import Numeric.Natural (Natural)

-- | Like 'Sum', but without any 'Num' constraints.
--
-- @since 1.0
newtype Additive (a :: Type) = Additive
  { -- | @since 1.0
    getAdditive :: a
  }
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
instance Semigroup (Additive Natural) where
  {-# INLINEABLE (<>) #-}
  Additive n <> Additive m = Additive $ n + m
  {-# INLINEABLE stimes #-}
  stimes i (Additive n)
    | i <= 0 = error "stimes for Additive Natural: expecting positive exponent."
    | otherwise = Additive . fromIntegral $ toInteger i * toInteger n

-- | @since 1.0
instance Monoid (Additive Natural) where
  {-# INLINEABLE mempty #-}
  mempty = Additive 0

-- | @since 1.0
instance Abelian (Additive Natural) where
  {-# INLINEABLE factor #-}
  Additive n `factor` Additive m =
    Additive <$> case compare n m of
      LT -> []
      EQ -> [0]
      GT -> [n - m]

-- | @since 1.0
instance Semigroup (Additive Integer) where
  {-# INLINEABLE (<>) #-}
  Additive n <> Additive m = Additive $ n + m
  {-# INLINEABLE stimes #-}
  stimes i (Additive n)
    | i <= 0 = error "stimes for Additive Integer: expecting positive exponent."
    | otherwise = Additive $ toInteger i * n

-- | @since 1.0
instance Monoid (Additive Integer) where
  {-# INLINEABLE mempty #-}
  mempty = Additive 0

-- | @since 1.0
instance Abelian (Additive Integer) where
  {-# INLINEABLE factor #-}
  Additive n `factor` Additive m = Additive <$> [n - m]

-- | @since 1.0
instance Semigroup (Additive Int8) where
  {-# INLINEABLE (<>) #-}
  Additive n <> Additive m = Additive $ n + m
  {-# INLINEABLE stimes #-}
  stimes i (Additive n)
    | i <= 0 = error "stimes for Additive Int8: expecting positive exponent."
    | otherwise = Additive . fromIntegral $ toInteger i * toInteger n

-- | @since 1.0
instance Monoid (Additive Int8) where
  {-# INLINEABLE mempty #-}
  mempty = Additive 0

-- | @since 1.0
instance Abelian (Additive Int8) where
  {-# INLINEABLE factor #-}
  Additive n `factor` Additive m = Additive <$> [n - m]

-- | @since 1.0
instance Semigroup (Additive Int16) where
  {-# INLINEABLE (<>) #-}
  Additive n <> Additive m = Additive $ n + m
  {-# INLINEABLE stimes #-}
  stimes i (Additive n)
    | i <= 0 = error "stimes for Additive Int16: expecting positive exponent."
    | otherwise = Additive . fromIntegral $ toInteger i * toInteger n

-- | @since 1.0
instance Monoid (Additive Int16) where
  {-# INLINEABLE mempty #-}
  mempty = Additive 0

-- | @since 1.0
instance Abelian (Additive Int16) where
  {-# INLINEABLE factor #-}
  Additive n `factor` Additive m = Additive <$> [n - m]

-- | @since 1.0
instance Semigroup (Additive Int32) where
  {-# INLINEABLE (<>) #-}
  Additive n <> Additive m = Additive $ n + m
  {-# INLINEABLE stimes #-}
  stimes i (Additive n)
    | i <= 0 = error "stimes for Additive Int32: expecting positive exponent."
    | otherwise = Additive . fromIntegral $ toInteger i * toInteger n

-- | @since 1.0
instance Monoid (Additive Int32) where
  {-# INLINEABLE mempty #-}
  mempty = Additive 0

-- | @since 1.0
instance Abelian (Additive Int32) where
  {-# INLINEABLE factor #-}
  Additive n `factor` Additive m = Additive <$> [n - m]

-- | @since 1.0
instance Semigroup (Additive Int64) where
  {-# INLINEABLE (<>) #-}
  Additive n <> Additive m = Additive $ n + m
  {-# INLINEABLE stimes #-}
  stimes i (Additive n)
    | i <= 0 = error "stimes for Additive Int64: expecting positive exponent."
    | otherwise = Additive . fromIntegral $ toInteger i * toInteger n

-- | @since 1.0
instance Monoid (Additive Int64) where
  {-# INLINEABLE mempty #-}
  mempty = Additive 0

-- | @since 1.0
instance Abelian (Additive Int64) where
  {-# INLINEABLE factor #-}
  Additive n `factor` Additive m = Additive <$> [n - m]

-- | @since 1.0
instance Semigroup (Additive Int) where
  {-# INLINEABLE (<>) #-}
  Additive n <> Additive m = Additive $ n + m
  {-# INLINEABLE stimes #-}
  stimes i (Additive n)
    | i <= 0 = error "stimes for Additive Int: expecting positive exponent."
    | otherwise = Additive . fromIntegral $ toInteger i * toInteger n

-- | @since 1.0
instance Monoid (Additive Int) where
  {-# INLINEABLE mempty #-}
  mempty = Additive 0

-- | @since 1.0
instance Abelian (Additive Int) where
  {-# INLINEABLE factor #-}
  Additive n `factor` Additive m = Additive <$> [n - m]
