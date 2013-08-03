{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified StackVM as SVM

newtype MinMax = MinMax Integer deriving (Show, Eq, Ord)
newtype Mod7 = Mod7 Integer deriving (Show, Eq)

-- Exercise 1
-- Write an evaluator for ExprT.
eval :: ExprT -> Integer
eval (Lit n)     = n
eval (Add x1 x2) = eval x1 + eval x2
eval (Mul x1 x2) = eval x1 * eval x2

-- Exercise 2
-- Evaluate given an unparsed string of an expression.
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

-- Exercise 3
reify :: ExprT -> ExprT
reify = id

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 x) (Mod7 y) = lit $ x + y
  mul (Mod7 x) (Mod7 y) = lit $ x * y

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4)"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

-- Exercise 5
instance Expr SVM.Program where
  lit x = [SVM.PushI x]
  add x y = x ++ y ++ [SVM.Add]
  mul x y = x ++ y ++ [SVM.Mul]

compile :: String -> Maybe SVM.Program
compile = parseExp lit add mul
