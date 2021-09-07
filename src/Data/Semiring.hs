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
import Data.Kind (Type)
import Data.Monoid.Commutative (Commutative)
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
  (Commutative (AddOf a), Monoid (MulOf a), Semirig a) =>
  Semiring (a :: Type)
  where
  fromNatural :: Natural -> a
  fromNatural = \case
    0 -> zero
    n -> coerce @(AddOf a) (stimes n mempty)

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
