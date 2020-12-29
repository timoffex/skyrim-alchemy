module Data.UPair
  ( UPair
  , pair
  , unpair
  ) where

-- | An unordered pair.
data UPair a
  = UPair { lesser :: a, greater :: a }
  deriving ( Show, Eq, Ord )

-- | Creates an unordered pair of values (which could be equal).
pair :: Ord a => a -> a -> UPair a
pair x y
  | x <= y    = UPair { lesser = x, greater = y }
  | otherwise = UPair { lesser = y, greater = x }

-- | Get the pair of values inside the 'UPair'.
--
-- The values are ordered with the first one '<=' the second.
unpair :: UPair a -> (a, a)
unpair (UPair x y) = (x, y)
