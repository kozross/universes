{-# LANGUAGE DerivingVia #-}
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
import Data.Yoneda (Yoneda (Yoneda), runYoneda)
import Prelude hiding ((*), (+))

-- | @since 1.0
newtype Free (a :: Type)
  = Free (Yoneda Semirig a)
  deriving
    ( -- | @since 1.0
      Functor,
      -- | @since 1.0
      Applicative,
      -- | @since 1.0
      Monad
    )
    via (Yoneda Semirig)

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

newtype MulFree (a :: Type) = MulFree a

instance Semigroup (MulFree (Free a)) where
  {-# INLINEABLE (<>) #-}
  MulFree (Free (Yoneda cb)) <> MulFree (Free (Yoneda cb')) =
    MulFree (Free (Yoneda (\f -> cb f * cb' f)))
