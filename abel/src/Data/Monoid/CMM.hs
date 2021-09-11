{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Monoid.CMM
  ( -- * Type class
    CMM (..),

    -- * Functions
    (^-),
  )
where

import Control.Applicative (Const (Const))
import Data.Functor.Identity (Identity (Identity))
import Data.IntSet (IntSet)
import qualified Data.IntSet as ISet
import Data.Kind (Type)
import Data.Semigroup (Max, Min, Sum (Sum))
import Data.Semigroup.Abelian (Abelian)
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)

-- | @since 1.0
class (Monoid m, Abelian m) => CMM (m :: Type) where
  -- | @since 1.0
  monus :: m -> m -> m

-- | @since 1.0
instance CMM () where
  {-# INLINEABLE monus #-}
  monus _ _ = ()

-- | \'Difference or zero\'.
--
-- @since 1.0
instance CMM (Sum Natural) where
  {-# INLINEABLE monus #-}
  monus (Sum n) (Sum m)
    | n <= m = Sum 0
    | otherwise = Sum (n - m)

-- | Set difference.
--
-- @since 1.0
instance (Ord a) => CMM (Set a) where
  {-# INLINEABLE monus #-}
  monus = Set.difference

-- | Set difference.
--
-- @since 1.0
instance CMM IntSet where
  {-# INLINEABLE monus #-}
  monus = ISet.difference

-- | Left argument if not greater, else maximal.
--
-- @since 1.0
instance (Ord a, Bounded a) => CMM (Min a) where
  {-# INLINEABLE monus #-}
  x `monus` y = case compare x y of
    LT -> x
    EQ -> x
    GT -> maxBound

-- | Left argument if greater, else minimal.
--
-- @since 1.0
instance (Ord a, Bounded a) => CMM (Max a) where
  {-# INLINEABLE monus #-}
  x `monus` y = case compare x y of
    LT -> minBound
    EQ -> minBound
    GT -> x

-- | @since 1.0
instance (CMM a) => CMM (Maybe a) where
  {-# INLINEABLE monus #-}
  x `monus` y = case (x, y) of
    (Nothing, _) -> Nothing
    (Just _, Nothing) -> x
    (Just x', Just y') -> Just $ x' `monus` y'

-- | @since 1.0
instance (CMM m) => CMM (Identity m) where
  {-# INLINEABLE monus #-}
  Identity x `monus` Identity y = Identity $ monus x y

-- | @since 1.0
instance (CMM a, CMM b) => CMM (a, b) where
  {-# INLINEABLE monus #-}
  (x, y) `monus` (x', y') = (x `monus` x', y `monus` y')

-- | @since 1.0
instance (CMM m) => CMM (Const m a) where
  {-# INLINEABLE monus #-}
  Const x `monus` Const y = Const $ x `monus` y

-- Helpers

-- | An infix synonym for 'monus'.
--
-- @since 1.0
(^-) ::
  forall (m :: Type).
  (CMM m) =>
  m ->
  m ->
  m
(^-) = monus
