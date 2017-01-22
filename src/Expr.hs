{-# LANGUAGE ExistentialQuantification #-}

module Expr where

import Search
import Supply
import qualified Data.Map.Strict as Map
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

-- Euclidian division. Also possible: use an exact division (only divide d/n
-- when d is a multiple of n).
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
  show (Div (Expr e1) (Expr e2)) = "(" ++ show e1 ++ "/" ++ show e2 ++ ")"
instance IsExpr Div where
  eval (Div (Expr e1) (Expr e2)) = (eval e1) `div` (eval e2)

data Fact = Fact Expr
instance Show Fact where
  show (Fact (Expr e)) = show e ++ "!"
instance IsExpr Fact where
  eval (Fact (Expr e)) = product [ 1 .. (eval e) ]

{-> fmap head . Map.filterWithKey (\k _ -> k>=0)  $ groupSearch 8 [Supply 1 (num 2) (Just 1), Supply 1 (num 0) (Just 1), Supply 1 (num 1) (Just 1), Supply 1 (num 7) (Just 1), Supply 0 plus Nothing, Supply 0 minus Nothing, Supply 0 mult Nothing, Supply 0 divide Nothing, Supply 0 (fact 20) Nothing] evalExpr

fromList [(0,(((0/7)/1)!!/2)),(1,(((0/7)/1)/2)!!),(2,((7-0)/(1+2))!!),(3,((7/(0!+1))!/2)),(4,(((0!!+7)/1)/2)),(5,(((7-2!)/0!)/1)),(6,(((7/0!)/1)/2)!),(7,((7/(0/2)!!)/1)),(8,((2!-(0!-7))/1)),(9,(((2!+7)/0!)/1)),(10,((0!!+(7+2))/1)),(11,((2!*(7-0!))-1)),(12,((2!*(7-0!))/1)),(13,(((2!*7)-0!)/1)),(14,(((2!*7)/0!)/1)),(15,((0!!+(7*2))/1)),(16,((2!*(0!+7))/1)),(17,(((0!+1)*2)!-7)),(18,((0!+1)!*(7+2))),(20,(((0!!+2)*7)-1)),(21,(((0!!+2)*7)/1)),(22,(0!!+((1+2)*7))),(23,(((0!+7)/2)!-1)),(24,(((0!+7)/1)/2)!),(25,(((0!+7)/2)!+1)),(28,((0!+1)!*(7*2))),(31,(((0!+1)*2)!+7)),(35,(((0!+2)!-1)*7)),(36,((0!+2)!*(7-1))),(41,(((0!+2)!*7)-1)),(42,(((0!+2)!*7)/1)),(43,(0!+((1+2)!*7))),(48,((0!+2)!*(7+1))),(49,(((0!+2)!+1)*7)),(60,(((7-0!)-1)!/2)),(102,(((1+2)-0)!!/7)),(118,(((7-0!)-1)!-2)),(119,(((7-2)!-0!)/1)),(120,(((7-2)/0!)/1)!),(121,(((7-2)!+0!)/1)),(122,(((7-0!)-1)!+2)),(168,(((0!+1)*2)!*7)),(240,((7-0!)!/(1+2))),(359,(((7-0!)!-1)/2)),(360,(((7-0!)/1)!/2)),(361,(0!+((7-1)!/2))),(713,(((1+2)-0)!!-7)),(717,((7-0!)!-(1+2))),(718,(((7-0!)!-2)/1)),(719,(((7-0)/2)!!-1)),(720,(((7-0)/1)/2)!!),(721,((7/2)!!-(0-1))),(722,(((7-0!)!+2)/1)),(723,((7-0!)!+(1+2))),(727,((1+2)!!-(0-7))),(840,((7-0)!/(1+2)!)),(1260,(7!/((0!+1)*2))),(1438,(((7-0!)!-1)*2)),(1439,(((7-0!)!*2)-1)),(1440,(((7-0!)!*2)/1)),(1441,(0!+((7-1)!*2))),(1442,(((7-0!)!+1)*2)),(1679,((7!-0!)/(1+2))),(1680,((7/0!)!/(1+2))),(1681,(0!+(7!/(1+2)))),(2160,((7-0!)!*(1+2))),(2518,((7!/(0!+1))-2)),(2519,(((7!-0!)/1)/2)),(2520,(((7/0!)/1)!/2)),(2521,((7!+(0!+1))/2)),(2522,(0!+((7!/2)+1))),(5034,((7-0)!-(1+2)!)),(5036,(7!-((0!+1)*2))),(5037,((7!-(0!+2))/1)),(5038,(((7!-2)/0!)/1)),(5039,((7!-(0/2)!)/1)),(5040,((7/(0/2)!)/1)!),(5041,((7!-(0!-2))/1)),(5042,(((7!+2)/0!)/1)),(5043,((7!+(0!+2))/1)),(5044,(7!+((0!+1)*2))),(5046,((1+2)!-(0-7!))),(10076,((7!-(0!+1))*2)),(10077,(((7!-0!)*2)-1)),(10078,(((7!-0!)*2)/1)),(10079,(((7!*2)-0!)/1)),(10080,(((7!*2)/0!)/1)),(10081,((0!+(7!*2))/1)),(10082,(((7!+0!)*2)/1)),(10083,(0!+((7!+1)*2))),(10084,((7!+(0!+1))*2)),(13440,((0!+7)!/(1+2))),(15117,((7!-0!)*(1+2))),(15119,((7!*(0!+2))-1)),(15120,((7!*(0!+2))/1)),(15121,(0!+(7!*(1+2)))),(15123,((7!+0!)*(1+2))),(20159,(((0!+7)!-1)/2)),(20160,(((0!+7)/1)!/2)),(20161,(0!+((7+1)!/2))),(30240,(((1+2)-0)!*7!)),(40317,((0!+7)!-(1+2))),(40318,(((0!+7)!-2)/1)),(40319,(((7+2)-0!)!-1)),(40320,(((7+2)-0!)/1)!),(40321,((0!+7)!-(1-2))),(40322,(((0!+7)!+2)/1)),(40323,((0!+7)!+(1+2))),(80638,(((0!+7)!-1)*2)),(80639,(((0!+7)!*2)-1)),(80640,(((0!+7)!*2)/1)),(80641,(0!+((7+1)!*2))),(80642,(((0!+7)!+1)*2)),(120960,((0!+7)!*(1+2))),(181440,((0!+(7+1))!/2)),(362878,((0!+(7+1))!-2)),(362879,(((7+2)!-0!)/1)),(362880,(((7+2)/0!)/1)!),(362881,(((7+2)!+0!)/1)),(362882,((0!+(7+1))!+2)),(725760,((0!+(7+1))!*2)),(3628799,((0!+(7+2))!-1)),(3628800,((0!+(7+2))/1)!),(3628801,((0!+(7+2))!+1)),(39916800,(((7-0!)*2)-1)!),(479001599,(((7-0!)*2)!-1)),(479001600,(((7-0!)*2)/1)!),(479001601,(((7-0!)*2)!+1)),(6227020799,(((7*2)-0!)!-1)),(6227020800,(((7*2)-0!)/1)!),(6227020801,(((7*2)-0!)!+1)),(43589145600,(((0!+1)*7)!/2)),(87178291198,(((0!+1)*7)!-2)),(87178291199,(((7*2)!-0!)/1)),(87178291200,(((7*2)/0!)/1)!),(87178291201,(((7*2)!+0!)/1)),(87178291202,(((0!+1)*7)!+2)),(174356582400,(((0!+1)*7)!*2)),(1307674367999,((0!+(7*2))!-1)),(1307674368000,((0!+(7*2))/1)!),(1307674368001,((0!+(7*2))!+1)),(20922789887999,(((0!+7)*2)!-1)),(20922789888000,(((0!+7)*2)/1)!),(20922789888001,(((0!+7)*2)!+1)),(355687428096000,(0!+((7+1)*2))!),(6402373705728000,((7-0!)*(1+2))!),(2432902008176640000,(((0!+2)*7)-1)!)]
-}
