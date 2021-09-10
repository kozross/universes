{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module Data.Semigroup.Abelian
  ( Abelian (..),
  )
where

import Data.Functor.Identity (Identity (Identity))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntSet (IntSet)
import qualified Data.IntSet as ISet
import Data.Kind (Type)
import Data.Semigroup (Min (Min), Sum (Sum))
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)

-- | @since 1.0
class (Eq s, Semigroup s) => Abelian (s :: Type) where
  -- | @since 1.0
  factor :: s -> s -> [s]

-- | @since 1.0
instance Abelian () where
  {-# INLINEABLE factor #-}
  factor _ _ = [()] -- yawn

-- | @since 1.0
instance Abelian (Sum Natural) where
  {-# INLINEABLE factor #-}
  Sum x `factor` Sum y =
    Sum <$> case compare x y of
      LT -> []
      EQ -> [0]
      GT -> [x - y]

-- | @since 1.0
instance Abelian (Sum Integer) where
  {-# INLINEABLE factor #-}
  Sum x `factor` Sum y = Sum <$> [x - y]

-- | @since 1.0
instance Abelian (Sum Int8) where
  {-# INLINEABLE factor #-}
  Sum x `factor` Sum y =
    Sum <$> [x - y]

-- | @since 1.0
instance Abelian (Sum Int16) where
  {-# INLINEABLE factor #-}
  Sum x `factor` Sum y =
    Sum <$> [x - y]

-- | @since 1.0
instance Abelian (Sum Int32) where
  {-# INLINEABLE factor #-}
  Sum x `factor` Sum y =
    Sum <$> [x - y]

-- | @since 1.0
instance Abelian (Sum Int64) where
  {-# INLINEABLE factor #-}
  Sum x `factor` Sum y =
    Sum <$> [x - y]

-- | @since 1.0
instance Abelian (Sum Int) where
  {-# INLINEABLE factor #-}
  Sum x `factor` Sum y =
    Sum <$> [x - y]

-- | @since 1.0
instance (Ord a) => Abelian (Set a) where
  {-# INLINEABLE factor #-}
  x `factor` y = [x `Set.difference` y | y `Set.isSubsetOf` x]

-- | @since 1.0
instance Abelian IntSet where
  {-# INLINEABLE factor #-}
  x `factor` y = [x `ISet.difference` y | y `ISet.isSubsetOf` x]

-- | @since 1.0
instance (Ord a) => Abelian (Min a) where
  {-# INLINEABLE factor #-}
  Min x `factor` Min y =
    Min <$> case compare x y of
      LT -> [x]
      EQ -> [x]
      GT -> []

-- | @since 1.0
instance (Abelian a) => Abelian (Maybe a) where
  {-# INLINEABLE factor #-}
  x `factor` y = case (x, y) of
    (Nothing, Nothing) -> [Nothing]
    (Nothing, Just _) -> []
    (Just x', Nothing) -> [Just x']
    (Just x', Just y') ->
      let factors = Just <$> factor x' y'
       in if x' == y'
            then Nothing : factors
            else factors

-- | @since 1.0
instance (Abelian a) => Abelian (Identity a) where
  {-# INLINEABLE factor #-}
  Identity x `factor` Identity y = Identity <$> factor x y

-- | @since 1.0
instance (Abelian a, Abelian b) => Abelian (a, b) where
  {-# INLINEABLE factor #-}
  (x, x') `factor` (y, y') = [(z, z') | z <- x `factor` y, z' <- x' `factor` y']
