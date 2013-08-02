module CIS194_Week4 where

import Data.List ((\\))

-- Exercise 1
-- fun1 :: [Integer] -> Integer
-- fun1 [] = 1
-- fun1 (x:xs)
--    | even x = (x - 2) * fun1 xs
--    | otherwise = fun1 xs
fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

-- fun2 :: Integer -> Integer
-- fun2 1 = 0
-- fun2 n
--    | even n = n + fun2 (n ‘div‘ 2)
--    | otherwise = fun2 (3 * n + 1)
fun2' :: Integer -> Integer
fun2' = last . takeWhile (/= 0) . iterate collatz

collatz :: Integer -> Integer
collatz 1 = 0
collatz n = if even n then n `div` 2 else 3 * n + 1

-- Exercise 2 (Incomplete)
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

insert :: a -> Tree a -> Tree a
insert n Leaf = Node 0 Leaf n Leaf
insert n (Node h left x right)
    | height left <= height right = Node h (insert n left) x right
    | otherwise                   = Node h left x (insert n right)
  where height Leaf = 0
        height (Node h' _ _ _) = h'

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr (\x acc -> if x then not acc else acc) False

-- Rewrite map in terms of foldr.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- Rewrite foldl in terms of foldr.
--
-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldr :: (a -> b -> b) -> b -> [a] -> b
--
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse

-- Exercise 4
-- Implement the Sieve of Sundaram.
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2 : (map (\x -> 2 * x + 1) $ [1 .. 2 * n + 2] \\ removeForm)
  where removeForm = [ i + j + 2 * i * j | (i, j) <- cartProd [1..n] [1..n]]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
