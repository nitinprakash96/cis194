module Week7.JoinList where

import Data.Monoid
import Week7.Buffer
import Week7.Editor
import Week7.Sized

data JoinList m a = Empty
                  | Single a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Show, Eq)
