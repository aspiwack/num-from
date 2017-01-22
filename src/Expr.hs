{-# LANGUAGE ExistentialQuantification #-}

module Expr where

import Search
import Supply
import Control.Monad (guard)

-- | Defines a class 'Expr' which characterise types which represent
-- expressions. For simplicity expressions are limited to representing integers.
class Show a => IsExpr a where
  eval :: a -> Integer

data Expr = forall a. IsExpr a => Expr a

instance Show Expr where
  show (Expr e) = show e

evalExpr :: Expr -> Integer
evalExpr (Expr e) = eval e

--- Some expressions constructions
num :: Integer -> Searching Expr Expr
num i = return $ Expr (I i)

plus :: Searching Expr Expr
plus = (\x y -> Expr (Plus x y)) <$> searchLoop <*> searchLoop

minus :: Searching Expr Expr
minus = (\x y -> Expr (Minus x y)) <$> searchLoop <*> searchLoop

mult :: Searching Expr Expr
mult = (\x y -> Expr (Mult x y)) <$> searchLoop <*> searchLoop

divide :: Searching Expr Expr
divide = do
  n <- searchLoop
  d <- searchLoop
  guard (evalExpr d /= 0)
  return $ Expr (Div n d)

-- Possible: add the precondition that the argument must be
-- non-negative. Probably required: limit the value of the argument to avoid
-- super-giant computations (hint 7!!!!!!!! is a big number…)
fact :: Searching Expr Expr
fact = (Expr . Fact) <$> searchLoop

data I = I Integer
instance Show I where
  show (I i) = show i
instance IsExpr I where
  eval (I i) = i

data Plus = Plus Expr Expr
instance Show Plus where
  show (Plus (Expr e1) (Expr e2)) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
instance IsExpr Plus where
  eval (Plus (Expr e1) (Expr e2)) = (eval e1)+(eval e2)

data Minus = Minus Expr Expr
instance Show Minus where
  show (Minus (Expr e1) (Expr e2)) = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
instance IsExpr Minus where
  eval (Minus (Expr e1) (Expr e2)) = (eval e1)-(eval e2)

data Mult = Mult Expr Expr
instance Show Mult where
  show (Mult (Expr e1) (Expr e2)) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"
instance IsExpr Mult where
  eval (Mult (Expr e1) (Expr e2)) = (eval e1)*(eval e2)

data Div = Div Expr Expr
instance Show Div where
  show (Div (Expr e1) (Expr e2)) = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
instance IsExpr Div where
  eval (Div (Expr e1) (Expr e2)) = (eval e1) `div` (eval e2)

data Fact = Fact Expr
instance Show Fact where
  show (Fact (Expr e)) = show e ++ "!"
instance IsExpr Fact where
  eval (Fact (Expr e)) = product [ 1 .. (eval e) ]

{-> fmap head $ groupSearch 7 [Supply (num 2) (Just 1), Supply (num 0) (Just 1), Supply (num 1) (Just 1), Supply (num 7) (Just 1), Supply plus Nothing, Supply minus Nothing, Supply mult Nothing, Supply divide Nothing] evalExpr

fromList [(-21,(((0-1)-2)*7)),(-16,(((0-7)-1)*2)),(-15,(((0-7)*2)-1)),(-14,(((0-7)*2)-1)),(-13,((1-(7*2))-0)),(-12,(((1-7)-0)*2)),(-10,(((0-7)-1)-2)),(-9,(((0-7)-2)-1)),(-8,(((0-1)-2)-7)),(-7,(((0-2)-7)-1)),(-6,((0-2)-(7-1))),(-5,(((2-7)-0)-1)),(-4,(((0-7)-1)-2)),(-3,((7-2)-(0-1))),(-2,(((0-7)-2)-1)),(-1,(((0-1)-7)-2)),(0,(((0-7)-1)-2)),(1,((0-7)-(1-2))),(2,((7-0)-(1+2))),(3,(((7-0)-1)-2)),(4,(((7+1)-0)-2)),(5,(((7-0)-2)-1)),(6,((0-2)-(1-7))),(7,((7-(0-2))-1)),(8,(((7+2)-1)-0)),(9,(((7+2)-0)-1)),(10,(((7+1)+2)-0)),(12,(((7-1)-0)*2)),(13,(((7-0)*2)-1)),(14,(((7-0)*2)-1)),(15,((7*2)-(0-1))),(16,(((7+1)-0)*2)),(21,(((1+2)-0)*7))]
-}
