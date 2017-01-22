module Supply where

import Control.Lens

-- | Denotes an available quantity of a value, 'Nothing' means that it's
-- infinitely available. Invariant: the 'quantity' must be at least one and
-- larger than the 'minq'.
data Supply a = Supply { minq :: Int, self :: a , quantity :: Maybe Int }
  deriving (Show)

-- | 'Nothing' if there is quantity would drop to 0.
takeOne :: Supply a -> Maybe (Supply a)
takeOne s =
  case quantity s of
    Nothing -> Just s
    Just i ->
      if i > 1 then Just $ s { minq = max (minq s -1) 0, quantity = Just (i-1) }
      else Nothing

selectOne :: [Supply a] -> [(a,[Supply a])]
selectOne [] = []
selectOne (s:ss) =
  this : (map (over _2 (s:)) (selectOne ss))
  where
    this = case takeOne s of
      Nothing -> ( self s , ss )
      Just s' -> ( self s , s':ss )
