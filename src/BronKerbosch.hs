-- | Implements the Bron-Kerbosch algorithm for finding the maximal
-- cliques in a graph.
module BronKerbosch
  ( bronKerbosch
  ) where


import           Data.Foldable
    ( Foldable (fold) )
import qualified Data.Map.Strict as M
import qualified Data.Set        as S


-- | Given an undirected graph represented by a map from nodes to
-- neighbors, outputs the maximal cliques in the graph.
bronKerbosch :: Ord a => M.Map a (S.Set a) -> [S.Set a]
bronKerbosch neighbors =
  bronKerboschImpl
    neighbors
    (S.fromList $ M.keys neighbors)
    S.empty
    S.empty


bronKerboschImpl
  :: ( Ord a )
  => M.Map a (S.Set a)
  -> S.Set a   -- ^ The @P@ set
  -> S.Set a   -- ^ The @R@ set
  -> S.Set a   -- ^ The @X@ set
  -> [S.Set a]
bronKerboschImpl neighbors p r x
  | S.null p && S.null x = [r]
  | otherwise =
      let
        (v:vs) = S.toList p
        nv = fold $ M.lookup v neighbors
      in if S.null p
         then []
         else bronKerboschImpl
                neighbors
                (S.intersection nv p)
                (S.insert v r)
                (S.intersection nv x) ++
              bronKerboschImpl
                neighbors
                (S.fromList vs)
                r
                (S.insert v x)
