{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module Data.Semigroup.Abelian
  ( Abelian (..),
  )
where

import Control.Applicative (Const (Const))
import Data.Functor.Identity (Identity (Identity))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntSet (IntSet)
import qualified Data.IntSet as ISet
import Data.Kind (Type)
import Data.Semigroup
  ( Max (Max),
    Min (Min),
    Product (Product),
    Sum (Sum),
  )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.These (These (That, These, This))
import Numeric.Natural (Natural)

-- | = Laws
--
-- 1. @x <> y = y <> x@ (commutativity of '<>')
-- 2. There exists @x, y@ such that @'not' '.' 'null' '$' 'factor' x y@
--    (non-triviality).
-- 3. If @'factor' x y = z@, then @'all' ((x '==') . (y '<>')) z@ and
--    @'all' ((x '==') . ('<>' y)) z@ (recombination).
-- 4. If @'factor' y x = factYX@ and @'factor' z y = factZY'@, then @'factor' z x
--    = ???@
--
-- If @s@ is also a 'Monoid', then the following law also holds:
--
-- 5. @'not' . 'null' '$' 'factor' x x@ (????)
--
-- @since 1.0
class (Eq s, Semigroup s) => Abelian (s :: Type) where
  -- | @'factor' x y@ produces a list of all @z@ such that @y '<>' z = z '<>' y
  -- = x@. There may be none, finitely many, or infinitely many, for any given
  -- choices of @x@ and @y@.
  --
  -- @since 1.0
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
instance Abelian (Product Natural) where
  {-# INLINEABLE factor #-}
  Product x `factor` Product y =
    Product <$> case (x, y) of
      (0, 0) -> [0 ..]
      (0, _) -> [0]
      (_, 0) -> []
      _ ->
        let (d, r) = y `quotRem` x
         in [d | r == 0]

-- | @since 1.0
instance Abelian (Sum Integer) where
  {-# INLINEABLE factor #-}
  Sum x `factor` Sum y = Sum <$> [x - y]

-- TODO: Show problem of 'every element' for Abelian (Product Integer)

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
instance (Ord a) => Abelian (Max a) where
  {-# INLINEABLE factor #-}
  Max x `factor` Max y =
    Max <$> case compare x y of
      LT -> []
      EQ -> [x]
      GT -> [x]

-- | @since 1.0
instance (Abelian a) => Abelian (Maybe a) where
  {-# INLINEABLE factor #-}
  x `factor` y = case (x, y) of
    (Nothing, Nothing) -> [Nothing]
    (Nothing, Just _) -> []
    (Just x', Nothing) -> Just x' : (Just <$> factor x' x')
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
  (x, x') `factor` (y, y') =
    [(z, z') | z <- x `factor` y, z' <- x' `factor` y']

-- | @since 1.0
instance (Abelian b) => Abelian (Const b a) where
  {-# INLINEABLE factor #-}
  Const x `factor` Const y = Const <$> factor x y

-- | @since 1.0
instance (Abelian a, Abelian b) => Abelian (These a b) where
  This x `factor` This y = This <$> factor x y
  That x `factor` That y = That <$> factor x y
  These x y `factor` These x' y' =
    These <$> factor x x' <*> factor y y'
  These x y `factor` This x' =
    let factors = (`These` y) <$> factor x x'
     in if x == x'
          then That y : factors
          else factors
  These x y `factor` That y' =
    let factors = These x <$> factor y y'
     in if y == y'
          then This x : factors
          else factors
  _ `factor` _ = []
