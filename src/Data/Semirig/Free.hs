{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Semirig.Free
  ( Free,
    interpret,
  )
where

import Data.Kind (Type)
import Data.Semigroup.Abelian (Abelian (factor))
import Data.Semirig (Semirig (AddOf, MulOf), (*), (+))
import Prelude hiding ((*), (+))

-- | @since 1.0
newtype Free (a :: Type)
  = Free (forall (s :: Type). (Semirig s) => (a -> s) -> s)

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
instance Monad Free where
  {-# INLINEABLE (>>=) #-}
  Free cb >>= f = Free (\g -> cb (\x -> let Free cb' = f x in cb' g))

-- | @since 1.0
instance Semirig (Free a) where
  type AddOf (Free a) = AddFree (Free a)
  type MulOf (Free a) = MulFree (Free a)

-- | \'Run\' the free 'Semirig' computation, given a way of turning @a@s into a
-- 'Semirig' of your choice.
--
-- @since 1.0
interpret ::
  forall (s :: Type) (a :: Type).
  (Semirig s) =>
  (a -> s) ->
  Free a ->
  s
interpret f (Free cb) = cb f

-- Helpers

newtype AddFree (a :: Type) = AddFree a

instance Semigroup (AddFree (Free a)) where
  {-# INLINEABLE (<>) #-}
  AddFree (Free cb) <> AddFree (Free cb') =
    AddFree (Free (\f -> cb f + cb' f))

-- Only the trivial factor is possible
instance Abelian (AddFree (Free a)) where
  {-# INLINEABLE factor #-}
  factor _ _ = []

newtype MulFree (a :: Type) = MulFree a

instance Semigroup (MulFree (Free a)) where
  {-# INLINEABLE (<>) #-}
  MulFree (Free cb) <> MulFree (Free cb') =
    MulFree (Free (\f -> cb f * cb' f))
