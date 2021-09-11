{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Semiring.Zhegalkin
  ( Zhegalkin (..),
  )
where

import Data.Bits (Bits)
import Data.Kind (Type)
import Data.Semigroup.Bitwise (BAnd (BAnd), BXor (BXor))
import Data.Semirig (Semirig (AddOf, MulOf))

-- | [Zhegalkin
-- polynomials](https://en.wikipedia.org/wiki/Zhegalkin_polynomial).
--
-- @since 1.0
newtype Zhegalkin (a :: Type) = Zhegalkin
  { -- | @since 1.0
    getZhegalkin :: a
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
instance (Bits a) => Semirig (Zhegalkin a) where
  type AddOf (Zhegalkin a) = BXor a
  type MulOf (Zhegalkin a) = BAnd a
