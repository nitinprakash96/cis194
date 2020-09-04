module Week1.CreditCard where


{-
 Exercise 1: We need to first find the digits of a number.

 1. toDigits should convert positive Integers to a list of digits. (For 0 or
 negative inputs, toDigits should return the empty list.)

 2. toDigitsRev should do the same, but with the digits reversed.

 Examples: toDigits 1234 == [1,2,3,4]
           toDigitsRev 1234 == [4,3,2,1]
           toDigits 0 == []
           toDigits (-17) == []
 -}
toDigits :: Integer -> [Integer]
toDigits n
  | n < 1     = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]


-- We can perhaps just use reverse function with toDigitsRev' to increase performace
-- when input integer is very large. This avoids calling ++ recursively.
toDigits' :: Integer -> [Integer]
toDigits' n = reverse $ toDigitsRev' n


-- Another implmenetation of the toDigits function that calls toDigitsUtil with
-- an empty (accumulator) list as an arg.
toDigits'' :: Integer -> [Integer]
toDigits'' n = toDigitsUtil n []
  where
    toDigitsUtil z xs
      | z <= 0    = xs
      | otherwise = toDigitsUtil (z `div` 10) $ (z `mod` 10) : xs


-- One implementation of toDigitsRev could just use the definition of toDigits
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n


-- Or we can rewrite toDigitsRev which would follows a similar pattern to toDigits
toDigitsRev' :: Integer -> [Integer]
toDigitsRev' n
  | n < 1     = []
  | otherwise = n `mod` 10 : toDigitsRev' (n `div` 10)


{-
 Exercise 2: Once we have the digits in the proper order, we need to
 double every other one.

Remember that doubleEveryOther should double every other number beginning from the right,
that is, the second-to-last, fourth-to-last,. . . numbers are doubled.

 Examples: doubleEveryOther [8,7,6,5] == [16,7,12,5]
           doubleEveryOther [1,2,3] == [1,4,3]

 Idea is to reverse the list first and then double every second element
 from the beginning. Once doubled, reverse the list again.
 -}
doubleEverOther :: [Integer] -> [Integer]
doubleEverOther = reverse . zipWith (*) (cycle [1, 2]) . reverse


-- An implementation of doubleEverOther using pattern matching
doubleEverOtherUtil' :: [Integer] -> [Integer]
doubleEverOtherUtil' [] = []
doubleEverOtherUtil' (x : []) = [x]
-- Multiply every second element of the deconstructed pair and call doubleEverOther'
-- recursively on the remaining list
doubleEverOtherUtil' (x:y:xs) = x : (2 * y) : doubleEverOtherUtil' xs


doubleEverOther' :: [Integer] -> [Integer]
doubleEverOther' = reverse . doubleEverOtherUtil' . reverse

{-
 Exercise 3 The output of doubleEveryOther has a mix of one-digit
 and two-digit numbers.
 Define the function:
   sumDigits :: [Integer] -> Integer
 to calculate the sum of all digits.

 Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
 -}
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)


{-
 Define the function:
   validate :: Integer -> Bool
 that indicates whether an Integer could be a valid credit card number.
 This will use all functions defined in the previous exercises.

 Example: validate 4012888888881881 = True
          validate 4012888888881882 = False
 -}
validate :: Integer -> Bool
validate = (0==) . (`mod` 10) . sumDigits . doubleEverOther . toDigits
