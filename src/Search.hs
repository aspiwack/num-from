{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Search where

import Supply
import Control.Applicative (Alternative)
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map

-- | The 'Int' is to serve as a depth limit, otherwise there is no way to
-- prevent the search to produce infinitely deep terms.
newtype Searching a t = Searching (StateT (Int,[Supply (Searching a a)]) [] t)
  deriving (Functor,Applicative,Monad,Alternative,MonadPlus
           ,MonadState (Int,[Supply (Searching a a)]))

searchLoop :: Searching a a
searchLoop = do
  (fuel,supply) <- get
  guard (fuel >= 0)
  let choices = selectOne supply
  let actions = map (\(a,s) -> put (fuel-1,s) >> a) choices
  msum actions

groupSearch :: Ord k => Int -> [Supply (Searching a a)] -> (a->k) -> Map.Map k [a]
groupSearch fuel supply eval =
  case searchLoop of
    Searching search ->
      let results = runStateT search (fuel,supply)
          values  = map fst results
      in Map.fromListWith (++) $ map (\x -> (eval x , [x])) values
