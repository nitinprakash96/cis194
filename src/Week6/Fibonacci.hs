module Week6.Fibonacci where

{-
PREFACE:
 Fibonacci numbers
 The Fibonacci numbers Fn are defined as the sequence of integers,
 beginning with 0 and 1, where every integer in the sequence is the
 sum of the previous two. That is,

      F0 = 0
      F1 = 1
      Fn = Fn−1 + Fn−2 (n ≥ 2)

 For example, the first fifteen Fibonacci numbers are
      0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, . . .

 It’s quite likely that you’ve heard of the Fibonacci numbers before.
 The reason they’re so famous probably has something to do with the
 simplicity of their definition combined with the astounding variety of
 ways that they show up in various areas of mathematics as well as art
 and nature.
-}

{-
 Exercise 1:

 Translate the above definition of Fibonacci numbers directly into a recursive
 function definition of type
      fib :: Integer -> Integer
 so that fib n computes the nth Fibonacci number Fn. Now use fib to define
 the infinite list of all Fibonacci numbers,
      fibs1 :: [Integer]

 (Hint: You can write the list of all positive integers as [0..].)
-}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


fibs1 :: [Integer]
fibs1 = map fib [0..]


-- Another way to write this using list comprehension
fibs1' :: [Integer]
fibs1' = [fib i | i <- [0..]]


{-
 Exercise 2:

 When I said “we” in the previous sentence I actually meant “you”.
 Your task for this exercise is to come up with more efficient implementation.
 Specifically, define the infinite list
     fibs2 :: [Integer]

 so that it has the same elements as fibs1, but computing the first n
 elements of fibs2 requires only O(n) addition operations. Be sure to
 use standard recursion pattern(s) from the Prelude as appropriate.
-}
fibs2 :: [Integer]
fibs2 = 0 : 1 : fib2 0 1
  where
    fib2 a b = (a + b) : fib2 b (a + b)


-- We can also rewrite it using list comprehension
fibs2' :: [Integer]
fibs2' = 0 : 1 : [fibs2' !! (i - 2) + fibs2' !! (i - 1) | i <- [2..]]


{-
 PREFACE:

 Streams:
 We can be more explicit about infinite lists by defining a type Stream
 representing lists that must be infinite. (The usual list type represents
 lists that may be infinite but may also have some finite length.)
 In particular, streams are like lists but with only a “cons” constructor—
 whereas the list type has two constructors, [] (the empty list) and
 (:) (cons), there is no such thing as an empty stream. So a stream is
 simply defined as an element followed by a stream.
-}

{-
 Exercise 3:

 • Define a data type of polymorphic streams, Stream.

 • Write a function to convert a Stream to an infinite list,
     streamToList :: Stream a -> [a]

 • To test your Stream functions in the succeeding exercises, it will be
 useful to have an instance of Show for Streams. However, if you put deriving
 Show after your definition of Stream, as one usually does, the resulting
 instance will try to print an entire Stream—which, of course, will never
 finish. Instead, you should make your own instance of Show for Stream,

    instance Show a => Show (Stream a) where show ...

 which works by showing only some prefix of a stream (say, the first 20 elements).
-}
data Stream a = Cons a (Stream a)


streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs


streamFromList :: [a] -> Stream a
streamFromList xs =
  case cycle xs of
    (x:xs') -> Cons x $ streamFromList xs'


instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList


{-
 Exercise 4:

 Let’s create some simple tools for working with Streams.

 • Write a function
    streamRepeat :: a -> Stream a
 which generates a stream containing infinitely many copies of the given element.

 • Write a function
    streamMap :: (a -> b) -> Stream a -> Stream b
 which applies a function to every element of a Stream.

 • Write a function
    streamFromSeed :: (a -> a) -> a -> Stream a
 which generates a Stream from a “seed” of type a, which is the first element of
 the stream, and an “unfolding rule” of type a -> a
 which specifies how to transform the seed into a new seed, to be used for
 generating the rest of the stream.
-}
streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x


streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs


streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f $ f x


{-
 Exercise 5:

 Now that we have some tools for working with streams, let’s create a few:
 • Define the stream
    nats :: Stream Integer
 which contains the infinite list of natural numbers 0, 1, 2, . . .

 • Define the stream
    ruler :: Stream Integer
 which corresponds to the ruler function
    0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . . .
 where the nth element in the stream (assuming the first element corresponds to n = 1)
 is the largest power of 2 which evenly divides n.

 Hint:
 define a function interleaveStreams which alternates the elements from
 two streams. Can you use this function to implement ruler in a clever way
 that does not have to do any divisibility testing?
-}
nats :: Stream Integer
nats = streamFromSeed (+1) 0


interleaveStream :: Stream a -> Stream a -> Stream a
interleaveStream (Cons x xs) ys = Cons x $ interleaveStream ys xs 

-- This is a very interesting problem.
{-
 Desired stream:
    0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3...
 which can be visualised as follows:
    (streamOf 0 ((streamOf 1) (streamOf 2 ...)))
 where interleave happens with every pair of streams. Therefore, it seems like
 interleaving happens recursively at every stage.
-}
ruler :: Stream Integer
ruler = recursiveInterleave streams
    where
        streams                         = streamMap streamRepeat nats
        recursiveInterleave (Cons s xs) = interleaveStream s $ recursiveInterleave xs
