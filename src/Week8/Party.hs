{-# OPTIONS_GHC -fno-warn-orphans #-}

module Week8.Party where

import Week8.Employee
import Data.Tree
import Data.Monoid


{-
 PREFACE:

 As the most junior employee at Calculators R Us, Inc., you are tasked
 with organizing the office Spring Break party. As with all party organizers,
 your goal is, of course, to maximize the amount of fun which  is had at the party.
 Since some people enjoy parties more than others, you have estimated the amount of
 fun which will be had by each employee. So simply summing together all these values
 should indicate the amount of fun which will be had at the party in total, right?

 . . . well, there’s one small problem. It is a well-known fact that anyone whose immediate boss
 is also at the party will not have any fun at all. So if all the company employees are at
 the party, only the CEO will have fun, and everyone else will stand around laughing
 nervously and trying to look natural while looking for their boss out of the corner of their eyes.

 Your job, then, is to figure out who to invite to the party in order to maximize the total amount of fun.
-}

--- Exercise 1 ---

{-
 A function
    glCons :: Employee -> GuestList -> GuestList

 which adds an Employee to the GuestList (updating the cached
 Fun score appropriately). Of course, in general this is impossible:
 the updated fun score should depend on whether the Employee
 being added is already in the list, or if any of their direct subordinates are in the list,
 and so on. For our purposes, though, you may assume that none of these special
 cases will hold: that is, glCons should simply add the new Employee and add their fun
 score without doing any kind of checks.
 -}
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp:emps) (empFun emp + fun)


{-
 A Monoid instance for GuestList.
 (How is the Monoid instance Note that this requires creating an
 supposed to work, you ask? You figure it out!)
-}
instance Semigroup GuestList where
  (<>) (GL emps1 fun1) (GL emps2 fun2) = GL (emps1 <> emps2) (fun1 + fun2)


instance Monoid GuestList where
  mempty  = GL [] 0
  mappend (GL emps1 fun1) (GL emps2 fun2) = GL (emps1 ++ emps2) (fun1 + fun2)


{-
 A function moreFun :: GuestList -> GuestList -> GuestList
 which takes two GuestLists and returns whichever one of them
 is more fun, i.e. has the higher fun score. (If the scores are equal it
 does not matter which is returned.)
-}
moreFun :: GuestList -> GuestList -> GuestList
moreFun (GL emps1 fun1) (GL emps2 fun2)
  | fun1 >= fun2 = GL emps1 fun1
  | otherwise    = GL emps2 fun2


{-
 The Data.Tree module from the standard Haskell libraries defines
 the type of “rose trees”, where each node stores a data element and
 has any number of children (i.e. a list of subtrees):
    data Tree a = Node {
        rootLabel :: a, -- label value
        subForest :: [Tree a] -- zero or more child trees
    }
 Strangely, Data.Tree does not define a fold for this type! Rectify the
 situation by implementing
   treeFold :: ... -> Tree a -> b

 (See if you can figure out what type(s) should replace the dots in
 the type of treeFold. If you are stuck, look back at the lecture notes
 from Week 7, or infer the proper type(s) from the remainder of this
 assignment.)
-}
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x xs) = f x $ map (treeFold f) xs
