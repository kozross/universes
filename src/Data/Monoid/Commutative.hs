{-# LANGUAGE FlexibleInstances #-}

module Data.Monoid.Commutative
  ( Commutative (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Nat
  ( Nat,
    Nat16,
    Nat32,
    Nat64,
    Nat8,
    NatSum,
  )
import Data.Relationship (Relationship (Both, LeftToRight, RightToLeft))
import Data.Semigroup (Sum (Sum))
import Numeric.Natural (Natural)

-- | @since 1.0
class (Monoid m) => Commutative m where
  natCompare :: m -> m -> Relationship

-- | @since 1.0
instance Commutative (Sum Natural) where
  {-# INLINEABLE natCompare #-}
  natCompare (Sum n) (Sum m) = case compare n m of
    LT -> LeftToRight
    EQ -> Both
    GT -> RightToLeft

-- | @since 1.0
instance Commutative (Sum Integer) where
  {-# INLINEABLE natCompare #-}
  natCompare _ _ = Both

-- | @since 1.0
instance Commutative (NatSum Nat8) where
  {-# INLINEABLE natCompare #-}
  natCompare _ _ = Both

-- | @since 1.0
instance Commutative (NatSum Nat16) where
  {-# INLINEABLE natCompare #-}
  natCompare _ _ = Both

-- | @since 1.0
instance Commutative (NatSum Nat32) where
  {-# INLINEABLE natCompare #-}
  natCompare _ _ = Both

-- | @since 1.0
instance Commutative (NatSum Nat64) where
  {-# INLINEABLE natCompare #-}
  natCompare _ _ = Both

-- | @since 1.0
instance Commutative (NatSum Nat) where
  {-# INLINEABLE natCompare #-}
  natCompare _ _ = Both

-- | @since 1.0
instance Commutative (Sum Int8) where
  {-# INLINEABLE natCompare #-}
  natCompare _ _ = Both

-- | @since 1.0
instance Commutative (Sum Int16) where
  {-# INLINEABLE natCompare #-}
  natCompare _ _ = Both

-- | @since 1.0
instance Commutative (Sum Int32) where
  {-# INLINEABLE natCompare #-}
  natCompare _ _ = Both

-- | @since 1.0
instance Commutative (Sum Int64) where
  {-# INLINEABLE natCompare #-}
  natCompare _ _ = Both

-- | @since 1.0
instance Commutative (Sum Int) where
  {-# INLINEABLE natCompare #-}
  natCompare _ _ = Both
