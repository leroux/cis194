module Golf where

import Data.List

skips :: [a] -> [[a]]
skips l = reverse $ skips' (length l - 1) l
  where skips' 0 xs = [xs]
        skips' n xs = takeEvery n xs : skips' (n - 1) xs

takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery 0 xs = xs
takeEvery n xs
    | (not . null) xs' = head xs' : takeEvery n (tail xs')
    | otherwise = []
  where xs' = drop n xs

localMaxima :: [Integer] -> [Integer]
localMaxima (x:xs@(y:z:_))
    | y > x && y > z = y : localMaxima xs
    | otherwise = localMaxima xs
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = (unlines . reverse) (buildhisto xs) ++ "==========\n0123456789"

buildhisto :: [Integer] -> [String]
buildhisto [] = []
buildhisto xs = histoline xs : buildhisto xs'
  where xs' = xs \\ [0..9]

histoline :: [Integer] -> String
histoline xs = map mark [0..9]
  where mark x | x `elem` xs = '*'
               | otherwise = ' '
