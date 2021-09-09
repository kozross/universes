{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Data.Yoneda
  ( Yoneda (Yoneda),
    runYoneda,
  )
where

import Data.Kind (Constraint, Type)

-- A variant of Yoneda with a constraint trapped inside
newtype Yoneda (c :: Type -> Constraint) (a :: Type)
  = Yoneda (forall (b :: Type). (c b) => (a -> b) -> b)

instance Functor (Yoneda c) where
  {-# INLINEABLE fmap #-}
  fmap f (Yoneda cb) = Yoneda (\g -> cb (g . f))

instance Applicative (Yoneda c) where
  {-# INLINEABLE pure #-}
  pure x = Yoneda (\g -> g x)
  {-# INLINEABLE (<*>) #-}
  Yoneda cbf <*> Yoneda cbx =
    Yoneda (\f -> cbf (\g -> cbx (f . g)))

instance Monad (Yoneda c) where
  {-# INLINEABLE (>>=) #-}
  Yoneda cb >>= f = Yoneda (\g -> cb (\x -> let Yoneda cb' = f x in cb' g))

runYoneda ::
  forall (c :: Type -> Constraint) (b :: Type) (a :: Type).
  (c b) =>
  (a -> b) ->
  Yoneda c a ->
  b
runYoneda f (Yoneda cb) = cb f
