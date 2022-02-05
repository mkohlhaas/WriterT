module Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

newtype WriterT w m a = WriterT (m (Tuple a w))

runWriterT :: ∀ w m a. WriterT w m a -> m (Tuple a w)
runWriterT (WriterT x) = x

instance functorWriterT :: Functor m => Functor (WriterT w m) where
  map f (WriterT x) = WriterT $ x <#> \(Tuple a w) -> Tuple (f a) w

instance applyWriterT :: (Semigroup w, Monad m) => Apply (WriterT w m) where
  apply (WriterT f) (WriterT x) = WriterT do
    Tuple f1 w1 <- f
    Tuple x1 w2 <- x
    pure $ Tuple (f1 x1) (w1 <> w2)

instance applicativeWriterT :: (Monad m, Monoid w) => Applicative (WriterT w m) where
  pure x = WriterT $ pure $ Tuple x mempty

instance bindWriterT :: (Monad m, Semigroup w) => Bind (WriterT w m) where
  bind (WriterT ma) f = WriterT do
     Tuple a w1 <- ma
     Tuple b w2 <- runWriterT $ f a
     pure $ Tuple b (w1 <> w2)

instance monadWriterT :: (Monad m, Monoid w) => Monad (WriterT w m)

main :: Effect Unit
main = do
  log "🍝"
