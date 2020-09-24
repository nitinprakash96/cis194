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

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }


---- Exercise 2 -----
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


---- Exercise 3 -----
invade :: Battlefield -> Rand StdGen Battlefield
invade = undefined


---- Exercise 4 ----
successProb :: Battlefield -> Rand StdGen Double
successProb = undefined


---- Exercise 5 ----
exactSuccessProb :: Battlefield -> Double
exactSuccessProb = undefined
