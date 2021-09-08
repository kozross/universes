{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Data.Monoid.Commutative.Free
  ( -- * Type
    Free,

    -- * Functions

    -- ** Queries
    length,

    -- ** General reducer
    interpret,
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus)
import Data.Kind (Type)
import Data.Monoid.Commutative (Commutative (natCompare))
import Data.Ord.Pre (PreOrd, WrappedCommutative (WrappedCommutative))
import Data.Relationship (Relationship (Both))
import Data.Semigroup (getSum)
import Numeric.Natural (Natural)
import Witherable (Filterable (catMaybes, filter, mapMaybe))
import Prelude hiding (length)

-- | The free commutative monoid. This is similar to a list, but with two major
-- differences:
--
-- 1. It /must/ be finite; and
-- 2. We can never observe its order.
--
-- @since 1.0
newtype Free (a :: Type)
  = Free (forall (m :: Type). (Commutative m) => (a -> m) -> m)
  deriving
    ( -- | @since 1.0
      PreOrd
    )
    via (WrappedCommutative (Free a))

-- | @since 1.0
instance Functor Free where
  {-# INLINEABLE fmap #-}
  fmap f (Free cb) = Free (\g -> cb (g . f))

-- | @since 1.0
instance Applicative Free where
  {-# INLINEABLE pure #-}
  pure x = Free (\g -> g x)
  {-# INLINEABLE (<*>) #-}
  Free cbf <*> Free cbx = Free (\f -> cbf (\g -> cbx (f . g)))

-- | @since 1.0
instance Alternative Free where
  {-# INLINEABLE empty #-}
  empty = mempty
  {-# INLINEABLE (<|>) #-}
  (<|>) = (<>)

-- | @since 1.0
instance Monad Free where
  {-# INLINEABLE (>>=) #-}
  Free cb >>= f = Free (\g -> cb (\x -> let Free cb' = f x in cb' g))

-- | @since 1.0
instance MonadPlus Free

-- | @since 1.0
instance MonadFail Free where
  {-# INLINEABLE fail #-}
  fail _ = empty

-- | @since 1.0
instance Semigroup (Free a) where
  {-# INLINEABLE (<>) #-}
  Free cb <> Free cb' = Free (\f -> cb f <> cb' f)

-- | @since 1.0
instance Monoid (Free a) where
  {-# INLINEABLE mempty #-}
  mempty = Free (const mempty)

-- | @since 1.0
instance Commutative (Free a) where
  {-# INLINEABLE natCompare #-}
  natCompare _ _ = Both

-- | This relies on the fact that 'mempty' is a neutral element.
--
-- @since 1.0
instance Filterable Free where
  {-# INLINEABLE mapMaybe #-}
  mapMaybe f (Free cb) = Free (\g -> cb (maybe mempty g . f))
  {-# INLINEABLE catMaybes #-}
  catMaybes (Free cb) = Free (cb . maybe mempty)
  {-# INLINEABLE filter #-}
  filter f (Free cb) = Free (\g -> cb (\x -> if f x then g x else mempty))

-- | @since 1.0
length :: forall (a :: Type). Free a -> Natural
length = getSum . interpret (const 1)

-- | \'Run\' the free 'Commutative', given a way of turning @a@s into a
-- 'Commutative' of your choice.
--
-- @since 1.0
interpret ::
  forall (m :: Type) (a :: Type).
  (Commutative m) =>
  (a -> m) ->
  Free a ->
  m
interpret f (Free cb) = cb f
