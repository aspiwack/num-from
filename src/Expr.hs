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

-- Possible improvement: for plus, mult, take AC into account (take a sorted
-- list of arguments instead of just a pair)
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
-- non-negative.
fact :: Integer -> Searching Expr Expr
fact upper_bound = do
  x <- searchLoop
  guard (evalExpr x <= upper_bound) -- avoid unreasonable computations: hint
                                    -- 7!!!!!!!! is a big number…
  return $ Expr (Fact x)

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

{-> fmap head $ groupSearch 6 [Supply (num 2) (Just 1), Supply (num 0) (Just 1), Supply (num 1) (Just 1), Supply (num 7) (Just 1), Supply plus Nothing, Supply minus Nothing, Supply mult Nothing, Supply divide Nothing, Supply (fact 100) Nothing] evalExpr

fromList [(-87178291200,(0-(2!*7)!)),(-87178291199,(1!-(7*2)!)),(-362880,(0-(2!+7)!)),(-362879,(1!-(7+2)!)),(-40320,(0-(1!+7)!)),(-40319,(0!-(7+1)!)),(-40318,(2!-(7+1)!)),(-10080,(7!*(0-2!))),(-10079,(1!-(7!*2))),(-10078,(2!*(1-7!))),(-5042,((0-7!)-2!)),(-5041,(1!-(7!+2))),(-5040,(7!-(1!-2))),(-5039,((0!-7!)-1)),(-5038,((2!-7!)-1)),(-5037,(1!-(7!-2))),(-5034,((1+2)!-7!)),(-2520,(7!-(0-2!))),(-2519,(1!-(7!-2))),(-720,(0-(7-2)!!)),(-719,(0!-(7-1)!)),(-718,(2!-(7-1)!)),(-713,(7-(1+2)!!)),(-120,(0-(7-2!)!)),(-119,(1!-(7-2)!)),(-21,(((0-1)-2)*7)),(-16,(((0-7)-1)*2)),(-15,(((0-7)*2)-1)),(-14,(((0-7)*2)-1)),(-13,(1!!-(7*2))),(-12,(2!!*(1-7))),(-10,(((0-7)-1)-2)),(-9,(((0-7)-2)-1)),(-8,(1!!-(7+2))),(-7,(((0-2)-7)-1)),(-6,((0!!-7)-1)),(-5,((2!!-7)-1)),(-4,(((0-7)-1)-2)),(-3,((7-2)-(0-1))),(-2,(2!!-(0-1))),(-1,(1!!-(0-7))),(0,(1!!!!-7)),(1,1!!!!!!),(2,2!!!!!!),(3,(((7-0)-1)-2)),(4,(((7+1)-0)-2)),(5,(((7-0)-2)-1)),(6,((7-1!)-2)!),(7,((7-0!!)-1)),(8,((0!!+7)-1)),(9,(((7+2)-0)-1)),(10,(((7+1)+2)-0)),(12,(2!!*(7-1))),(13,((2!!*7)-1)),(14,((2!!*7)-1)),(15,((7*2)-(0-1))),(16,(2!!*(7+1))),(21,(((1+2)-0)*7)),(24,((1!+7)-2)!),(42,((1!+2)!*7)),(102,((1+2)!!-7)),(119,((7-2!)!-1)),(120,((7-2!)-1)!),(121,((7-2!)!+1)),(360,((7-1!)!-2)),(713,((1+2)!!-7)),(718,((7-1!)!-2)),(719,((7-2)!!-1)),(720,((7-1)-2)!!),(721,((7-2)!!+1)),(722,((7-1!)!+2)),(727,((1+2)!!+7)),(840,(7!-(1+2)!)),(1440,((7-1!)!*2)),(1680,(7!-(1!+2))),(2519,((7!-1!)-2)),(2520,((7-1!)!-2)),(2521,(1!+(7!-2))),(5034,(7!-(1+2)!)),(5037,(7!-(1!+2))),(5038,((7!-2!)-1)),(5039,((7!-0!)-1)),(5040,((7-0!)-1)!),(5041,((7!+0!)-1)),(5042,((7!+2!)-1)),(5043,(7!+(1!+2))),(5046,((1+2)!+7!)),(10078,(2!*(7!-1))),(10079,((7!*2!)-1)),(10080,((7!*2!)-1)),(10081,(1!+(7!*2))),(10082,(2!*(7!+1))),(15120,(7!*(1!+2))),(20160,((1!+7)!-2)),(30240,((1+2)!*7!)),(40318,((1!+7)!-2)),(40319,((0!+7)!-1)),(40320,((0!+7)-1)!),(40321,((0!+7)!+1)),(40322,((1!+7)!+2)),(80640,((1!+7)!*2)),(362879,((2!+7)!-1)),(362880,((2!+7)-1)!),(362881,((2!+7)!+1)),(3628800,(1!+(7+2))!),(479001600,(2!*(7-1))!),(6227020800,((2!*7)-1)!),(87178291199,((2!*7)!-1)),(87178291200,((2!*7)-1)!),(87178291201,((2!*7)!+1)),(1307674368000,(1!+(7*2))!),(20922789888000,(2!*(7+1))!),(51090942171709440000,((1!+2)*7)!),(620448401733239439360000,((7+1)-2)!!),(1405006117752879898543142606244511569936384000000000,((1+2)!*7)!)]
-}
