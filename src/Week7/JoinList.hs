{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Week7.JoinList where

import Data.Monoid
import Week7.Buffer
import Week7.Editor
import Week7.Sized
import Week7.Scrabble

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


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i jl
  | i >= 0 =
    case jl of
      Empty -> Empty
      Single _ _ -> Empty
      Append _ jl1 jl2
        | size' == i -> jl2
        | size' <  i -> dropJ (i - size') jl2
        | otherwise -> Append tag' jl1' jl2
        where size' = getSize $ size $ tag jl1
              jl1'  = dropJ i jl1
              tag'  = tag jl1 <> tag jl2
  | otherwise = jl


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n jl
    | n >= 0 =
        case jl of
          Empty -> Empty
          Single t a -> Single t a
          Append _ jl1 jl2
            | size' == n    -> jl1
            | size' >  n    -> takeJ n jl1
            | otherwise     -> Append tag' jl1 jl2'
            where size' = getSize $ size $ tag jl1
                  jl2'  = takeJ (n - size') jl2
                  tag'  = tag jl1 <> tag jl2'
    | otherwise = Empty


----- Exercise 3 ----
scoreLine :: String -> JoinList Score String
scoreLine [] = Empty
scoreLine xs = Single (scoreString xs) xs


----- Exercise 4 ----
instance Buffer (JoinList (Score, Size) String) where
  toString Empty          = ""
  toString (Single _ x)   = x
  toString (Append _ x y) = toString x ++ toString y

  fromString [] = Empty
  fromString xs = Single (scoreString xs, 1) xs

  line = indexJ

  replaceLine i str jl = takeJ i jl +++ newJl +++ dropJ (i + 1) jl
      where
        newJl = Single (scoreString str, 1) str

  numLines jl = getSize $ size $ tag jl

  value Empty               = 0
  value (Single (x, _) _)   = getScore x
  value (Append (x, _) _ _) = getScore x


enlist :: String -> JoinList (Score, Size) String
enlist s = Single (scoreString s, Size 1) s

buf :: JoinList (Score, Size) String
buf = (fromString . unlines)
      [ "This buffer is for notes you don't want to save, and for"
      , "evaluation of steam valve coefficients."
      , "To load a different file, type the character L followed"
      , "by the name of the file."
      ]

main :: IO ()
main = runEditor editor $ buf
