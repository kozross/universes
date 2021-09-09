{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Semiring.Free
  ( Free,
    interpret,
  )
where

import Data.Kind (Type)
import Data.Semigroup.Abelian (Abelian (factor))
import Data.Semirig (Semirig (AddOf, MulOf), (*), (+))
import Data.Semiring (Semiring, one, zero)
import Data.Yoneda (Yoneda (Yoneda), runYoneda)
import Prelude hiding ((*), (+))

-- | @since 1.0
newtype Free (a :: Type)
  = Free (Yoneda Semiring a)
  deriving
    ( -- | @since 1.0
      Functor,
      -- | @since 1.0
      Applicative,
      -- | @since 1.0
      Monad
    )
    via (Yoneda Semiring)

-- | @since 1.0
instance Semirig (Free a) where
  type AddOf (Free a) = AddFree (Free a)
  type MulOf (Free a) = MulFree (Free a)

-- | @since 1.0
instance Semiring (Free a)

-- | \'Run\' the free 'Semiring' computation, given a way of turning @a@s into a
-- 'Semiring' of your choice.
--
-- @since 1.0
interpret ::
  forall (s :: Type) (a :: Type).
  (Semiring s) =>
  (a -> s) ->
  Free a ->
  s
interpret f (Free y) = runYoneda f y

-- Helpers

newtype AddFree (a :: Type) = AddFree a

instance Semigroup (AddFree (Free a)) where
  {-# INLINEABLE (<>) #-}
  AddFree (Free (Yoneda cb)) <> AddFree (Free (Yoneda cb')) =
    AddFree (Free (Yoneda (\f -> cb f + cb' f)))

-- Only the trivial factor is possible
instance Abelian (AddFree (Free a)) where
  {-# INLINEABLE factor #-}
  factor _ _ = []

instance Monoid (AddFree (Free a)) where
  {-# INLINEABLE mempty #-}
  mempty = AddFree (Free (Yoneda (const zero)))

newtype MulFree (a :: Type) = MulFree a

instance Semigroup (MulFree (Free a)) where
  {-# INLINEABLE (<>) #-}
  MulFree (Free (Yoneda cb)) <> MulFree (Free (Yoneda cb')) =
    MulFree (Free (Yoneda (\f -> cb f * cb' f)))

instance Monoid (MulFree (Free a)) where
  {-# INLINEABLE mempty #-}
  mempty = MulFree (Free (Yoneda (const one)))
