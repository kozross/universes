{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module Data.Semigroup.Intersectional
  ( Intersectional (..),
  )
where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as ISet
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (stimes, stimesIdempotent)
import Data.Set (Set)
import qualified Data.Set as Set

-- | @since 1.0
newtype Intersectional (a :: Type) = Intersectional a
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
instance (Ord a) => Semigroup (Intersectional (Set a)) where
  {-# INLINEABLE (<>) #-}
  Intersectional s1 <> Intersectional s2 =
    Intersectional (s1 `Set.intersection` s2)
  {-# INLINEABLE stimes #-}
  stimes = stimesIdempotent

-- | @since 1.0
instance Semigroup (Intersectional IntSet) where
  {-# INLINEABLE (<>) #-}
  Intersectional s1 <> Intersectional s2 =
    Intersectional (s1 `ISet.intersection` s2)
  {-# INLINEABLE stimes #-}
  stimes = stimesIdempotent

-- | @since 1.0
instance (Ord k, Semigroup v) => Semigroup (Intersectional (Map k v)) where
  {-# INLINEABLE (<>) #-}
  Intersectional m1 <> Intersectional m2 =
    Intersectional (Map.intersectionWith (<>) m1 m2)
  {-# INLINEABLE stimes #-}
  stimes = stimesIdempotent

-- | @since 1.0
instance (Semigroup v) => Semigroup (Intersectional (IntMap v)) where
  {-# INLINEABLE (<>) #-}
  Intersectional im1 <> Intersectional im2 =
    Intersectional (IMap.intersectionWith (<>) im1 im2)
  {-# INLINEABLE stimes #-}
  stimes = stimesIdempotent
