{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Ord.Pre
  ( -- * Type class
    PreOrd (..),

    -- * Helper types
    WrappedOrd (..),
    WrappedCommutative (..),
  )
where

import Data.IntSet (IntSet)
import qualified Data.IntSet as ISet
import Data.Kind (Type)
import Data.Monoid.Commutative (Commutative (natCompare))
import Data.Relationship
  ( Relationship
      ( Both,
        LeftToRight,
        RightToLeft,
        Unrelatable
      ),
  )
import Data.Set (Set)
import qualified Data.Set as Set

-- | @since 1.0
class PreOrd (a :: Type) where
  preCompare :: a -> a -> Relationship

-- | By inclusion.
--
-- @since 1.0
instance (Ord a) => PreOrd (Set a) where
  {-# INLINEABLE preCompare #-}
  preCompare = inclusion

-- | By inclusion.
--
-- @since 1.0
instance PreOrd IntSet where
  {-# INLINEABLE preCompare #-}
  preCompare = inclusion'

-- | @since 1.0
newtype WrappedOrd (a :: Type) = WrappedOrd a
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

-- | A total order is also a preorder.
--
-- @since 1.0
instance (Ord a) => PreOrd (WrappedOrd a) where
  {-# INLINEABLE preCompare #-}
  preCompare (WrappedOrd x) (WrappedOrd y) = case compare x y of
    EQ -> Both
    LT -> LeftToRight
    GT -> RightToLeft

-- | @since 1.0
newtype WrappedCommutative (m :: Type) = WrappedCommutative m
  deriving
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Ord,
      -- | @since 1.0
      Semigroup,
      -- | @since 1.0
      Monoid
    )
    via m
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | Any commutative monoid has a natural preorder.
--
-- @since 1.0
instance (Commutative m) => PreOrd (WrappedCommutative m) where
  {-# INLINEABLE preCompare #-}
  preCompare (WrappedCommutative x) (WrappedCommutative y) =
    natCompare x y

-- Helpers

inclusion ::
  forall (a :: Type).
  (Ord a) =>
  Set a ->
  Set a ->
  Relationship
inclusion s1 s2 = case (Set.isSubsetOf s1 s2, Set.isSubsetOf s2 s1) of
  (False, False) -> Unrelatable -- disjoint sets are incomparable
  (False, True) -> RightToLeft
  (True, False) -> LeftToRight
  (True, True) -> Both

inclusion' :: IntSet -> IntSet -> Relationship
inclusion' s1 s2 = case (ISet.isSubsetOf s1 s2, ISet.isSubsetOf s2 s1) of
  (False, False) -> Unrelatable -- disjoint sets are incomparable
  (False, True) -> RightToLeft
  (True, False) -> LeftToRight
  (True, True) -> Both
