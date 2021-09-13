{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module Data.Semigroup.Multiplicative (Multiplicative (..)) where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Semigroup (stimes)
import Data.Semigroup.Abelian (Abelian (factor))
import Numeric.Natural (Natural)

-- | Like 'Product', but without any 'Num' constraints.
--
-- @since 1.0
newtype Multiplicative (a :: Type) = Multiplicative
  { -- | @since 1.0
    getMultiplicative :: a
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
instance Semigroup (Multiplicative Natural) where
  {-# INLINEABLE (<>) #-}
  Multiplicative n <> Multiplicative m = Multiplicative $ n * m
  {-# INLINEABLE stimes #-}
  stimes i (Multiplicative n)
    | i <= 0 = error "stimes for Multiplicative Natural: expecting positive exponent."
    | otherwise = Multiplicative . fromIntegral $ toInteger n ^ toInteger i

-- | @since 1.0
instance Monoid (Multiplicative Natural) where
  {-# INLINEABLE mempty #-}
  mempty = Multiplicative 1

-- | @since 1.0
instance Abelian (Multiplicative Natural) where
  {-# INLINEABLE factor #-}
  Multiplicative n `factor` Multiplicative m =
    Multiplicative <$> case (n, m) of
      (0, 0) -> [0 ..]
      (0, _) -> [0]
      (_, 0) -> []
      _ ->
        let (d, r) = n `quotRem` m
         in [d | r == 0]

-- | @since 1.0
instance Semigroup (Multiplicative Integer) where
  {-# INLINEABLE (<>) #-}
  Multiplicative n <> Multiplicative m = Multiplicative $ n * m
  {-# INLINEABLE stimes #-}
  stimes i (Multiplicative n)
    | i <= 0 = error "stimes for Multiplicative Integer: expecting positive exponent."
    | otherwise = Multiplicative $ n ^ toInteger i

-- | @since 1.0
instance Monoid (Multiplicative Integer) where
  {-# INLINEABLE mempty #-}
  mempty = Multiplicative 1

-- | @since 1.0
instance Abelian (Multiplicative Integer) where
  {-# INLINEABLE factor #-}
  Multiplicative n `factor` Multiplicative m =
    Multiplicative <$> case (n, m) of
      (0, 0) -> mconcat . zipWith (\x y -> [x, y]) [0 ..] $ [-1, -2 ..]
      (0, _) -> [0]
      (_, 0) -> []
      _ ->
        let (d, r) = n `quotRem` m
         in [d | r == 0]

-- | @since 1.0
instance Semigroup (Multiplicative Int8) where
  {-# INLINEABLE (<>) #-}
  Multiplicative n <> Multiplicative m = Multiplicative $ n * m
  {-# INLINEABLE stimes #-}
  stimes i (Multiplicative n)
    | i <= 0 = error "stimes for Multiplicative Int8: expecting positive exponent."
    | otherwise = Multiplicative . fromIntegral $ toInteger n ^ toInteger i

-- | @since 1.0
instance Monoid (Multiplicative Int8) where
  {-# INLINEABLE mempty #-}
  mempty = Multiplicative 1

-- | @since 1.0
instance Abelian (Multiplicative Int8) where
  {-# INLINEABLE factor #-}
  Multiplicative n `factor` Multiplicative m =
    Multiplicative <$> case (n, m) of
      (0, 0) -> mconcat . zipWith (\x y -> [x, y]) [0 ..] $ [-1, -2 ..]
      (0, _) -> [0]
      (_, 0) -> []
      _ ->
        let (d, r) = n `quotRem` m
         in [d | r == 0]

-- | @since 1.0
instance Semigroup (Multiplicative Int16) where
  {-# INLINEABLE (<>) #-}
  Multiplicative n <> Multiplicative m = Multiplicative $ n * m
  {-# INLINEABLE stimes #-}
  stimes i (Multiplicative n)
    | i <= 0 = error "stimes for Multiplicative Int16: expecting positive exponent."
    | otherwise = Multiplicative . fromIntegral $ toInteger n ^ toInteger i

-- | @since 1.0
instance Monoid (Multiplicative Int16) where
  {-# INLINEABLE mempty #-}
  mempty = Multiplicative 1

-- | @since 1.0
instance Abelian (Multiplicative Int16) where
  {-# INLINEABLE factor #-}
  Multiplicative n `factor` Multiplicative m =
    Multiplicative <$> case (n, m) of
      (0, 0) -> mconcat . zipWith (\x y -> [x, y]) [0 ..] $ [-1, -2 ..]
      (0, _) -> [0]
      (_, 0) -> []
      _ ->
        let (d, r) = n `quotRem` m
         in [d | r == 0]

-- | @since 1.0
instance Semigroup (Multiplicative Int32) where
  {-# INLINEABLE (<>) #-}
  Multiplicative n <> Multiplicative m = Multiplicative $ n * m
  {-# INLINEABLE stimes #-}
  stimes i (Multiplicative n)
    | i <= 0 = error "stimes for Multiplicative Int32: expecting positive exponent."
    | otherwise = Multiplicative . fromIntegral $ toInteger n ^ toInteger i

-- | @since 1.0
instance Monoid (Multiplicative Int32) where
  {-# INLINEABLE mempty #-}
  mempty = Multiplicative 1

-- | @since 1.0
instance Abelian (Multiplicative Int32) where
  {-# INLINEABLE factor #-}
  Multiplicative n `factor` Multiplicative m =
    Multiplicative <$> case (n, m) of
      (0, 0) -> mconcat . zipWith (\x y -> [x, y]) [0 ..] $ [-1, -2 ..]
      (0, _) -> [0]
      (_, 0) -> []
      _ ->
        let (d, r) = n `quotRem` m
         in [d | r == 0]

-- | @since 1.0
instance Semigroup (Multiplicative Int64) where
  {-# INLINEABLE (<>) #-}
  Multiplicative n <> Multiplicative m = Multiplicative $ n * m
  {-# INLINEABLE stimes #-}
  stimes i (Multiplicative n)
    | i <= 0 = error "stimes for Multiplicative Int64: expecting positive exponent."
    | otherwise = Multiplicative . fromIntegral $ toInteger n ^ toInteger i

-- | @since 1.0
instance Monoid (Multiplicative Int64) where
  {-# INLINEABLE mempty #-}
  mempty = Multiplicative 1

-- | @since 1.0
instance Abelian (Multiplicative Int64) where
  {-# INLINEABLE factor #-}
  Multiplicative n `factor` Multiplicative m =
    Multiplicative <$> case (n, m) of
      (0, 0) -> mconcat . zipWith (\x y -> [x, y]) [0 ..] $ [-1, -2 ..]
      (0, _) -> [0]
      (_, 0) -> []
      _ ->
        let (d, r) = n `quotRem` m
         in [d | r == 0]

-- | @since 1.0
instance Semigroup (Multiplicative Int) where
  {-# INLINEABLE (<>) #-}
  Multiplicative n <> Multiplicative m = Multiplicative $ n * m
  {-# INLINEABLE stimes #-}
  stimes i (Multiplicative n)
    | i <= 0 = error "stimes for Multiplicative Int: expecting positive exponent."
    | otherwise = Multiplicative . fromIntegral $ toInteger n ^ toInteger i

-- | @since 1.0
instance Monoid (Multiplicative Int) where
  {-# INLINEABLE mempty #-}
  mempty = Multiplicative 1

-- | @since 1.0
instance Abelian (Multiplicative Int) where
  {-# INLINEABLE factor #-}
  Multiplicative n `factor` Multiplicative m =
    Multiplicative <$> case (n, m) of
      (0, 0) -> mconcat . zipWith (\x y -> [x, y]) [0 ..] $ [-1, -2 ..]
      (0, _) -> [0]
      (_, 0) -> []
      _ ->
        let (d, r) = n `quotRem` m
         in [d | r == 0]
