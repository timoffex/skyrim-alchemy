module PairMap
  ( PairMap
  -- * Construction
  , empty
  , insertPair

  -- * Modification
  , deletePair
  , delete
  , adjustPair
  , adjust

  -- * Querying
  , lookupPair
  , lookup
  , assocs
  ) where

import           Data.Foldable
import qualified Data.Map.Strict as M
import           GHC.Conc
import           Prelude         hiding
    ( lookup )

-- | A map from unordered pairs of keys to values.
data PairMap k a
  = PairMap
    { lesser  :: M.Map k (M.Map k a)
    , greater :: M.Map k (M.Map k a) }


empty :: Ord k => PairMap k a
empty = PairMap M.empty M.empty


insertPair :: Ord k => k -> k -> a -> PairMap k a -> PairMap k a
insertPair k1 k2 a pm =
  let
    (lesserK, greaterK) = if k1 <= k2
                          then (k1, k2)
                          else (k2, k1)
  in PairMap
     { lesser = M.alter ( Just
                        . maybe
                            (M.singleton greaterK a)
                            (M.insert greaterK a) )
                lesserK $ lesser pm
     , greater = M.alter ( Just
                         . maybe
                             (M.singleton lesserK a)
                             (M.insert lesserK a) )
                 greaterK $ greater pm }

deletePair :: Ord k => k -> k -> PairMap k a -> PairMap k a
deletePair k1 k2 pm =
  let (lk, gk) = if k1 <= k2 then (k1, k2) else (k2, k1)
  in PairMap
     { lesser = M.update (nothingIfEmpty . M.delete gk) lk (lesser pm)
     , greater = M.update (nothingIfEmpty . M.delete lk) gk (greater pm)
     }

delete :: Ord k => k -> PairMap k a -> PairMap k a
delete k pm =
  PairMap
  { lesser = M.filter (not . M.null) $
      M.map (M.delete k) $
      M.delete k $
      lesser pm
  , greater = M.filter (not . M.null) $
      M.map (M.delete k) $
      M.delete k $
      greater pm
  }

adjustPair :: Ord k => (a -> a) -> k -> k -> PairMap k a -> PairMap k a
adjustPair f k1 k2 pm =
  let
    (lk, gk) = if k1 <= k2 then (k1, k2) else (k2, k1)
    lesser' = M.adjust (M.adjust f gk) lk $ lesser pm
    greater' = M.adjust (M.adjust f lk) gk $ greater pm

    -- By convention, the 'lesser' map is used for querying, so we can
    -- compute the 'greater' map a little later (in parallel)
    --
    -- 1. Tell GHC to "spark" greater' to evaluate it in parallel, hoping
    --    that its result won't be needed too soon.
    -- 2. Tell GHC to fully evaluate lesser'.
    -- 3. Return the result.
    --
    -- NOTE: Parallelizing here is probably inefficient for the small
    -- maps that I expect, but I just want to try it anyway.
  in greater' `par` lesser' `pseq` PairMap
     { lesser = lesser'
     , greater = greater'
     }


adjust :: Ord k => (M.Map k a -> M.Map k a) -> k -> PairMap k a -> PairMap k a
adjust f k pm =
  let (lesserPart, greaterPart) = split k $ f $ lookup k pm
  in PairMap
     { lesser = M.alter (const $ nothingIfEmpty lesserPart) k $ lesser pm
     , greater = M.alter (const $ nothingIfEmpty greaterPart) k $ greater pm
     }


lookupPair :: Ord k => k -> k -> PairMap k a -> Maybe a
lookupPair k1 k2 pm =
  let (lk, gk) = if k1 <= k2 then (k1, k2) else (k2, k1)
  in M.lookup lk (lesser pm) >>= M.lookup gk

lookup :: Ord k => k -> PairMap k a -> M.Map k a
lookup k pm = M.union
                (fold $ M.lookup k $ lesser pm)
                (fold $ M.lookup k $ greater pm)




split :: Ord k => k -> M.Map k a -> (M.Map k a, M.Map k a)
split k m = ( M.filterWithKey (\k' _ -> k <= k') m
            , M.filterWithKey (\k' _ -> k > k') m )


nothingIfEmpty :: M.Map k a -> Maybe (M.Map k a)
nothingIfEmpty m = if M.null m then Nothing else Just m


assocs :: Ord k => PairMap k a -> [((k, k), a)]
assocs pm = do
  (k1, others) <- M.assocs (lesser pm)
  (k2, a) <- M.assocs others
  let (lk, gk) = if k1 <= k2 then (k1, k2) else (k2, k1)
  return ((lk, gk), a)
