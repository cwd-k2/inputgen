{-# LANGUAGE DeriveFunctor #-}
module Free where

data Free f a
  = Pure a
  | Join (f (Free f a))
  deriving Functor

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> a = f <$> a
  Join f <*> a = Join ((<*> a) <$> f)

instance Functor f => Monad (Free f) where
  Pure a >>= f = f a
  Join m >>= f = Join ((>>= f) <$> m)

liftF :: Functor f => f r -> Free f r
liftF = Join . fmap Pure
