{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}

module Data.Semigroup.Handed
  ( Lefty (..),
    Righty (..),
  )
where

import Data.Kind (Type)

-- | An 'Either' with a \'Left-ward\' bias.
--
-- @since 1.0
newtype Lefty (a :: Type) (b :: Type) = Lefty (Either a b)
  deriving
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Ord
    )
    via (Either a b)
  deriving stock
    ( -- | @since 1.0
      Show
    )
  deriving
    ( -- | @since 1.0
      Functor
    )
    via (Either a)

-- | @since 1.0
instance (Semigroup a, Semigroup b) => Semigroup (Lefty a b) where
  {-# INLINEABLE (<>) #-}
  Lefty x <> Lefty y = Lefty $ case (x, y) of
    (Left x', Left y') -> Left (x' <> y')
    (Left _, _) -> x
    (Right _, Left _) -> y
    (Right x', Right y') -> Right (x' <> y')

-- | @since 1.0
instance (Semigroup a, Monoid b) => Monoid (Lefty a b) where
  {-# INLINEABLE mempty #-}
  mempty = Lefty . Right $ mempty

-- TODO: Show 'present whole type' problem with Abelian (Lefty a b).

-- | An 'Either' with a \'Right-ward\' bias.
--
-- @since 1.0
newtype Righty (a :: Type) (b :: Type) = Righty (Either a b)
  deriving
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Ord
    )
    via (Either a b)
  deriving stock
    ( -- | @since 1.0
      Show
    )
  deriving
    ( -- | @since 1.0
      Functor
    )
    via (Either a)

-- | @since 1.0
instance (Semigroup a, Semigroup b) => Semigroup (Righty a b) where
  {-# INLINEABLE (<>) #-}
  Righty x <> Righty y = Righty $ case (x, y) of
    (Left x', Left y') -> Left (x' <> y')
    (Right x', Right y') -> Right (x' <> y')
    (Left _, Right _) -> y
    (Right _, Left _) -> x

-- | @since 1.0
instance (Monoid a, Semigroup b) => Monoid (Righty a b) where
  {-# INLINEABLE mempty #-}
  mempty = Righty . Left $ mempty

-- TODO: Show 'present whole type' problem with Abelian (Righty a b).
