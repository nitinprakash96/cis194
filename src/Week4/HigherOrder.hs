module Week4.HigherOrder where


{-
 Exercise 1: Wholemeal programming

 Reimplement each of the following functions in a more idiomatic
 Haskell style. Use wholemeal programming practices, breaking each
 function into a pipeline of incremental transformations to an entire
 data structure. Name your functions fun1’ and fun2’ respectively.

 1. fun1 :: [Integer] -> Integer
    fun1 [] = 1
    fun1 (x:xs)
      | even x = (x - 2) * fun1 xs
      | otherwise = fun1 xs

 2. fun2 :: Integer -> Integer
    fun2 1 = 0
    fun2 n | even n = n + fun2 (n ‘div‘ 2)
           | otherwise = fun2 (3 * n + 1)

 Hint: For this problem you may wish to use the functions iterate
 and takeWhile. Look them up in the Prelude documentation to see
 what they do
-}
fun1 :: [Integer] -> Integer
fun1 = product . map (+(-2)) . filter even


fun2 :: Integer -> Integer
fun2 = sum
  . filter even
  . takeWhile (>1)
  . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)


{-
 Exercise 2: Folding with trees

 Recall the definition of a binary tree data structure.
 The height of http://en.wikipedia.org/wiki/ Binary_tree a binary tree is the
 length of a path from the root to the deepest node. For example,
 the height of a tree with a single node is 0; the height of a tree with
 three nodes, whose root has two children, is 1; and so on. A binary tree
 is balanced if the height of its left and right subtrees differ by no more
 than 1, and its left and right subtrees are also balanced.

 You should use the following data structure to represent binary
 trees. Note that each node stores an extra Integer representing the height at that node.

 data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

 For this exercise, write a function
  foldTree :: [a] -> Tree a
  foldTree = ...
 which generates a balanced binary tree from a list of values using foldr.

 For example, one sample output might be the following,

 foldTree "ABCDEFGHIJ" ==
   Node 3
     (Node 2
       (Node 0 Leaf ’F’ Leaf)
       ’I’
       (Node 1 (Node 0 Leaf ’B’ Leaf) ’C’ Leaf))
     ’J’
     (Node 2
       (Node 1 (Node 0 Leaf ’A’ Leaf) ’G’ Leaf)
       ’H’
       (Node 1 (Node 0 Leaf ’D’ Leaf) ’E’ Leaf))

 Your solution might not place the nodes in the same exact order,
 but it should result in balanced trees, with each subtree having a
 correct computed height.
-}
-- data type to capture a tree
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)
