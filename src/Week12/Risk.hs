{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Week12.Risk where

import Data.List
import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show


{-
 Exercise 2:

 Given the definitions
   type Army = Int
   data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

 (which are also included in Risk.hs), write a function with the type
   battle :: Battlefield -> Rand StdGen Battlefield

 which simulates a single battle (as explained above) between two
 opposing armies. That is, it should simulate randomly rolling the
 appropriate number of dice, interpreting the results, and updating
 the two armies to reflect casualties. You may assume that each player
 will attack or defend with the maximum number of units they are allowed
-}
battle :: Battlefield -> Rand StdGen Battlefield
battle battlefield = do
  x <- rollDie attackers
  y <- rollDie defenders
  let results = zipWith (>) x y

  -- Apply list of True/False results on Battlefield
  return $ foldr bodyCount battlefield results
    where
      Battlefield v s = battlefield
      attackers = min 3 $ v - 1
      defenders = min 2 s

rollDie :: Int -> Rand StdGen [Int]
rollDie n = map unDV . sortDesc <$> dice n
  where
    sortDesc = reverse . sort
    dice n' = replicateM n' die

bodyCount :: Bool -> Battlefield -> Battlefield
bodyCount vic (Battlefield x y) =
  if vic
  then Battlefield  x (y - 1)
  else Battlefield (x - 1) y


{-
 Exercise 3:

 Of course, usually an attacker does not stop after just a single
 battle, but attacks repeatedly in an attempt to destroy the entire defending
 army (and thus take over its territory).

 Now implement a function
    invade :: Battlefield -> Rand StdGen Battlefield

 which simulates an entire invasion attempt, that is, repeated calls
 to battle until there are no defenders remaining, or fewer than two
 attackers.
-}
invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield a d)
  | a < 2 || d <= 0 = return b
  | otherwise       = battle b >>= invade


{-
 Exercise 4

 Finally, implement a function
    successProb :: Battlefield -> Rand StdGen Double
 which runs invade 1000 times, and uses the results to compute a
 Double between 0 and 1 representing the estimated probability that
 the attacking army will completely destroy the defending army.

 For example, if the defending army is destroyed in 300 of the 1000
 simulations (but the attacking army is reduced to 1 unit in the other 700),
 successProb should return 0.3
-}
successProb :: Battlefield -> Rand StdGen Double
successProb x = do
  invaded <- replicateM 1000 (invade x)
  let invasionSuccess = length $ filter id $ isVictory <$> invaded
  return $ fromIntegral invasionSuccess / 1000
  where
    isVictory :: Battlefield -> Bool
    isVictory (Battlefield a d) = a > d


main :: IO ()
main = do
  let battlefield = Battlefield 9 9
  print =<< evalRandIO (battle battlefield)
  print =<< evalRandIO (invade battlefield)
  print =<< evalRandIO (successProb battlefield)
