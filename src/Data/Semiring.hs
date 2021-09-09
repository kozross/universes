{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Semiring
  ( -- * Type class
    Semiring (..),

    -- * Functions
    zero,
    one,
  )
where

import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Nat (Nat, Nat16, Nat32, Nat64, Nat8)
import Data.Semigroup (stimes)
import Data.Semirig (Semirig (AddOf, MulOf))
import Numeric.Natural (Natural)

-- | = Laws
--
-- * @'zero' '*' x = x '*' 'zero' = 'zero'@ (annihilation)
-- * TODO: homomorphism from Natural
--
-- @since 1.0
class
  (Monoid (AddOf a), Monoid (MulOf a), Semirig a) =>
  Semiring (a :: Type)
  where
  fromNatural :: Natural -> a
  fromNatural = \case
    0 -> zero
    n -> coerce @(AddOf a) (stimes n mempty)

-- | @since 1.0
instance Semiring Natural

-- | @since 1.0
instance Semiring Nat8

-- | @since 1.0
instance Semiring Nat16

-- | @since 1.0
instance Semiring Nat32

-- | @since 1.0
instance Semiring Nat64

-- | @since 1.0
instance Semiring Nat

-- | @since 1.0
instance Semiring Integer

-- | @since 1.0
instance Semiring Int8

-- | @since 1.0
instance Semiring Int16

-- | @since 1.0
instance Semiring Int32

-- | @since 1.0
instance Semiring Int64

-- | @since 1.0
instance Semiring Int

-- | The additive identity.
--
-- @since 1.0
zero :: forall (a :: Type). (Semiring a) => a
zero = coerce @(AddOf a) mempty

-- | The multiplicative identity.
--
-- @since 1.0
one :: forall (a :: Type). (Semiring a) => a
one = coerce @(MulOf a) mempty
