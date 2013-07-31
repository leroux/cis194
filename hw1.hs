module CIS194_Week1 where

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = lastDigit : toDigitsRev initDigits
  where lastDigit = x `mod` 10
        initDigits = x `div` 10

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' . reverse
  where doubleEveryOther' []         = []
        doubleEveryOther' [x]        = [x]
        doubleEveryOther' (x:y:zs) = x : y * 2 : doubleEveryOther' zs

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigitsRev)

-- Exercise 4
validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)

-- Exercise 6
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 src dest _ = [(src, dest)]
hanoi n src dest spare =
    hanoi (n - 1) src spare dest ++ (src, dest) : hanoi (n - 1) spare dest src
