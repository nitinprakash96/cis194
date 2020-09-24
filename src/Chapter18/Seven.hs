module Chapter18.Seven where


import Test.QuickCheck

-- Welcome to the Nope Monad, where nothing happens and nobody cares.
data Nope a = NopeDotJpg deriving (Show)


instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _  = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  _ >>= _  = NopeDotJpg


-- naming these as Left' and Right' to remove ambiguity
data PhhhbtEither a b = Left' a | Right' b
  deriving (Eq, Show)


instance Functor (PhhhbtEither a) where
  fmap _ (Left' a)  = Left' a
  fmap f (Right' b) = Right' $ f b

instance Applicative (PhhhbtEither a) where
  pure = Right'
  (<*>) (Left' a) _ = Left' a
  (<*>) _ (Left' a) = Left' a
  (<*>) (Right' f) (Right' x) = Right' (f x)

instance Monad (PhhhbtEither a) where
  return = Right' -- Since Monads are supposed to have applicative instances,
                  -- can this be written as return = pure?
  (>>=) (Left' a) _  = Left' a
  (>>=) (Right' b) f = f b


newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
  return = Identity
  (Identity a) >>= f = f a


data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = append (fmap f xs) (fs <*> xs)


instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons a Nil) f = f a
  (>>=) (Cons a rest) f = f a `append` (rest >>= f)
