{-# LANGUAGE DerivingStrategies #-}

module Data.Relationship (Relationship (..)) where

-- | Describes the state of a binary relation between two elements.
--
-- @since 1.0
data Relationship
  = -- | The elements are unrelated.
    Unrelatable
  | -- | The left element relates to the right only.
    LeftToRight
  | -- | The right element relates to the left only.
    RightToLeft
  | -- | Both elements relate to each other in both directions
    Both
  deriving stock
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Show
    )
