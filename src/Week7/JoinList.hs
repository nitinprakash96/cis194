module Week7.JoinList where

import Data.Monoid
import Week7.Buffer
import Week7.Editor
import Week7.Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Show, Eq)


---- Exercise 1 ----
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b


---- Exercise 2 ----
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i j
  | i >= 0 =
    case j of
      Empty      -> Nothing
      Single _ a -> Just a
      Append _ j1 j2
        | size' > i -> indexJ i j1
        | otherwise -> indexJ (i - size') j2
        where size' = getSize $ size $ tag j1
  | otherwise = Nothing


