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
    ( Foldable (fold) )
import           Data.Function
    ( (&) )
import qualified Data.Map.Strict as M
import           Data.UPair
    ( UPair, pair, unpair )
import           GHC.Conc
    ( par, pseq )
import           Prelude         hiding
    ( lookup )

-- | A map from unordered pairs of keys to values.
data PairMap k a
  = PairMap
    { lesserToGreater :: M.Map k (M.Map k a)
    , greaterToLesser :: M.Map k (M.Map k a) }


empty :: Ord k => PairMap k a
empty = PairMap M.empty M.empty


insertPair :: Ord k => UPair k -> a -> PairMap k a -> PairMap k a
insertPair p a pm =
  let (lk, gk) = unpair p
  in PairMap
     { lesserToGreater =
         M.alter
           ( Just . maybe (M.singleton gk a) (M.insert gk a) )
           lk
           (lesserToGreater pm)
     , greaterToLesser =
         M.alter
           (Just . maybe (M.singleton lk a) (M.insert lk a))
           gk
           (greaterToLesser pm) }

deletePair :: Ord k => UPair k -> PairMap k a -> PairMap k a
deletePair p pm =
  let (lk, gk) = unpair p
  in PairMap
     { lesserToGreater =
         M.update (nothingIfEmpty . M.delete gk) lk (lesserToGreater pm)
     , greaterToLesser =
         M.update (nothingIfEmpty . M.delete lk) gk (greaterToLesser pm)
     }

delete :: Ord k => k -> PairMap k a -> PairMap k a
delete k pm =
  PairMap
  { lesserToGreater =
      lesserToGreater pm &
      M.delete k &
      M.map (M.delete k) &
      M.filter (not . M.null)
  , greaterToLesser =
      greaterToLesser pm &
      M.delete k &
      M.map (M.delete k) &
      M.filter (not . M.null)
  }

adjustPair :: Ord k => (a -> a) -> UPair k -> PairMap k a -> PairMap k a
adjustPair f p pm =
  let
    (lk, gk) = unpair p
    ltg' = M.adjust (M.adjust f gk) lk $ lesserToGreater pm
    gtl' = M.adjust (M.adjust f lk) gk $ greaterToLesser pm

    -- By convention, the 'ltg' map is used for querying, so we can
    -- compute the 'gtl' map a little later (in parallel)
    --
    -- 1. Tell GHC to "spark" gtl' to evaluate it in parallel, hoping
    --    that its result won't be needed too soon.
    -- 2. Tell GHC to fully evaluate lesser'.
    -- 3. Return the result.
    --
    -- NOTE: Parallelizing here is probably inefficient for the small
    -- maps that I expect, but I just want to try it anyway.
  in gtl' `par` ltg' `pseq` PairMap
     { lesserToGreater = ltg'
     , greaterToLesser = gtl'
     }


adjust :: Ord k => (M.Map k a -> M.Map k a) -> k -> PairMap k a -> PairMap k a
adjust f k pm =
  let (lesserPart, greaterPart) = split k $ f $ lookup k pm
  in PairMap
     { lesserToGreater =
         M.alter (const $ nothingIfEmpty lesserPart) k $ lesserToGreater pm
     , greaterToLesser =
         M.alter (const $ nothingIfEmpty greaterPart) k $ greaterToLesser pm
     }


lookupPair :: Ord k => UPair k -> PairMap k a -> Maybe a
lookupPair p pm =
  let (lk, gk) = unpair p
  in M.lookup lk (lesserToGreater pm) >>= M.lookup gk

lookup :: Ord k => k -> PairMap k a -> M.Map k a
lookup k pm = M.union
                (fold $ M.lookup k $ lesserToGreater pm)
                (fold $ M.lookup k $ greaterToLesser pm)




split :: Ord k => k -> M.Map k a -> (M.Map k a, M.Map k a)
split k m = ( M.filterWithKey (\k' _ -> k <= k') m
            , M.filterWithKey (\k' _ -> k > k') m )


nothingIfEmpty :: M.Map k a -> Maybe (M.Map k a)
nothingIfEmpty m = if M.null m then Nothing else Just m


assocs :: Ord k => PairMap k a -> [(UPair k, a)]
assocs pm = do
  (k1, others) <- M.assocs (lesserToGreater pm)
  (k2, a) <- M.assocs others
  return (pair k1 k2, a)
