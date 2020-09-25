module Chapter18.Seven where


import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Write Monad instances for the following types. Use the QuickCheck properties
-- we showed you to validate your instances.

{-
 Exercise 1:
 Welcome to the Nope Monad, where nothing happens and nobody cares.
    data Nope a = NopeDotJpg
-}
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _    = NopeDotJpg
  (<*>) _ _ = NopeDotJpg
  
instance Monad Nope where
  return _   = NopeDotJpg
  ( >>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq


{-
 Exercise 2:
 Monad instance for
    data PhhhbbtttEither b a = Left a | Right b
-}
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
  return = pure
  (>>=) (Left' a) _  = Left' a
  (>>=) (Right' b) f = f b

genEither :: (Arbitrary a, Arbitrary b) => Gen (PhhhbtEither a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left' a, Right' b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbtEither a b) where
  arbitrary = genEither

instance (Eq a, Eq b) => EqProp (PhhhbtEither a b) where
  (=-=) = eq


{-
 Exercise 3:
 Write a Monad instance for Identity.
    newtype Identity a = Identity a deriving (Eq, Ord, Show)

 instance Functor Identity where
   fmap = undefined
 instance Applicative Identity where
   pure = undefined
   (<*>) = undefined
 instance Monad Identity where
   return = pure
   (>>=) = undefined
-}
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

{-
 Exercise 4:
 This one should be easier than the Applicative instance was.
 Remember to use the Functor that Monad requires, then see where the chips fall.
   data List a = Nil
     | Cons a (List a)
-}
data List a = Nil | Cons a (List a)
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

genList :: Arbitrary a => Gen (List a)
genList = do
  a <- arbitrary
  l <- genList
  frequency [ (1, return Nil)
            , (2, return (Cons a l)) ]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

instance Eq a => EqProp (List a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = take' 1000 xs
              ys' = take' 1000 ys

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)


testWithTrigger trigger = do
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger


monadSpec :: IO ()
monadSpec = do
  testWithTrigger (undefined :: Nope (Int, Int, Int))
  testWithTrigger (undefined :: PhhhbtEither String (Int, Int, Int))
  testWithTrigger (undefined :: Identity (Int, Int, Int))
  testWithTrigger (undefined :: List (String, String, String))
