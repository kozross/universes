module Data.Monoid.Commutative
  ( Commutative (..),
  )
where

import Data.Relationship (Relationship)

-- | @since 1.0
class (Monoid m) => Commutative m where
  natCompare :: m -> m -> Relationship
