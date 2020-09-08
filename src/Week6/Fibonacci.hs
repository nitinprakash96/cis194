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
-- fibs2 :: [Integer]
