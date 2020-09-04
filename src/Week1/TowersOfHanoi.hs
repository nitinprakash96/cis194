module Week1.TowersOfHanoi where

{-
 Exercise 5 The Towers of Hanoi is a classic puzzle with a solution
 that can be described recursively. Disks of different sizes are stacked
 on three pegs; the goal is to get from a starting configuration with
 all disks stacked on the first peg to an ending configuration with all
 disks stacked on the last peg.

 The only rules are
  1. you may only move one disk at a time, and
  2. a larger disk may never be stacked on top of a smaller one.

 For example, as the first move all you can do is move the topmost,
 smallest disk onto a different peg, since only one disk may be moved
 at a time.

 To move n discs (stacked in increasing size) from peg a to peg b
 using peg c as temporary storage,
  1. move n − 1 discs from a to c using b as temporary storage
  2. move the top disc from a to b
  3. move n − 1 discs from c to b using a as temporary storage
-}
type Peg  = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 x y _ = [(x, y)]
hanoi n x y z = hanoi (n - 1) x z y ++ hanoi 1 x y z ++ hanoi (n - 1) z y x
