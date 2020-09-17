{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

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
        streams :: Stream (Stream Integer)
        streams                         = streamMap streamRepeat nats

        recursiveInterleave :: Stream (Stream Integer) -> Stream Integer
        recursiveInterleave (Cons s xs) = interleaveStream s $ recursiveInterleave xs


{-
 PREFACE: Fibonacci numbers via generating functions (extra credit)

 This section is optional but very cool, so if you have time I hope you
 will try it. We will use streams of Integers to compute the Fibonacci
 numbers in an astounding way.

 The essential idea is to work with generating functions of the form
     a0 + a1x + a2x2 + · · · + anxn + . . .

 where x is just a “formal parameter” (that is, we will never actually
 substitute any values for x; we just use it as a placeholder) and all the
 coefficients ai are integ
-}


---- EXERCISE 6 ------

{-
 First, define
    x :: Stream Integer

 by noting that x = 0 + 1x + 0x2 + 0x3 + . . .
-}
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0


{-
 Define an instance of the Num type class for Stream Integer.
 Note that you will have to add {-# LANGUAGE FlexibleInstances #-} to the top of your .hs file in order for
 this instance to be allowed.

 Here’s what should go in your Num instance:

 – You should implement the fromInteger function. Note that
     n = n + 0x + 0x2 + 0x3 + . . . .

 – You should implement negate: to negate a generating function, negate all its coefficients.

 – You should implement (+), which works like you would expect:
     (a0 + a1x + a2x2 + . . .) + (b0 + b1x + b2x2 + . . .) = (a0 + b0) + (a1 + b1)x + (a2 + b2)x2 + . . .

 – Multiplication is a bit trickier. Suppose A = a0 + xA0 and B = b0 + xB0 are two generating functions
   we wish to multiply.
 We reason as follows:
   AB = (a0 + xA0)B
      = a0B + xA0B
      = a0(b0 + xB0) + xA0B
      = a0b0 + x(a0B0 + A0B)

 That is, the first element of the product AB is the product of the first elements, a0b0; the remainder
 of the coefficient stream (the part after the x) is formed by multiplying every element in B 0
 (that is, the tail of B) by a0, and to

 Note that there are a few methods of the Num class I have not told you to implement, such as abs
 and signum. ghc will complain that you haven’t defined them, but don’t worry about it. We won’t
 need those methods. (To turn off these warnings you can add {-# OPTIONS_GHC -fno-warn-missing-methods #-}
 to the top of your file.) If you have implemented the above correctly, you should be able
 to evaluate things at the ghci prompt such as
     *Main> x^4
     *Main> (1 + x)^5
     *Main> (x^2 + x + 3) * (x - 5)
-}
instance Num (Stream Integer) where
  fromInteger i                 = Cons i $ streamRepeat 0
  negate (Cons v s)             = Cons (-v) $ negate s
  (+) (Cons v1 s1) (Cons v2 s2) = Cons (v1 + v2) (s1 + s2)
  (*) (Cons v1 s1) (Cons v2 s2) = Cons (v1 * v2) $ streamMap (* v1) s2 + s1 * Cons v2 s2


{-
 The penultimate step is to implement an instance of the Fractional
 class for Stream Integer. Here the important method to define is division, (/).
 I won’t bother deriving it (though it isn’t hard), but it turns out that if A = a0 + xA0 and
 B = b0 + xB0, then A/B = Q, where Q is defined as
     Q = (a0/b0) + x((1/b0)(A0 − QB0)).

 That is, the first element of the result is a0/b0; the remainder is formed by computing A0 − QB0
 and dividing each of its elements by b0.
 Of course, in general, this operation might not result in a stream of Integers. However, we will
 only be using this instance in cases
-}
instance Fractional (Stream Integer) where
  (/) (Cons v1 s1) (Cons v2 s2) = quo
    where
      first     = v1 `div` v2
      divider   = \i -> i `div` v2
      remainder = s1 - quo * s2
      quo       = Cons first $ streamMap divider remainder


{-
 Consider representing the Fibonacci numbers using a generating function,
    F(x) = F0 + F1x + F2x2 + F3x3 + . . .

 Notice that x + xF(x) + x2F(x) = F(x):
               x
               F0x + F1x2 + F2x3 + F3x4 + . . .
                     F0x2 + F1x3 + F2x4 + . . .
        ----------------------------------------
       0 + x + F2x2 + F3x3 + F4x4 + . . .

 Thus x = F(x) − xF(x) − x2F(x), and solving for F(x) we find that
          F(x) = x / (1 − x − x2).

 Translate this into an (amazing, totally sweet) definition
      fibs3 :: Stream Integer
-}
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)
