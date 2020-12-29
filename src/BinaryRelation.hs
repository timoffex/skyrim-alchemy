module BinaryRelation
  ( BinaryRelation
  , empty
  , swap
  , toSet
  , fromList
  , insert
  , delete
  , deleteLeft
  , deleteRight
  , check
  , byLeft
  , byRight
  , lefts
  , rights
  ) where


import           Data.Foldable
    ( Foldable (foldl') )
import qualified Data.Multimap.Set as SM
import qualified Data.Set          as S
import qualified Data.Tuple        as Tuple
    ( swap )


data BinaryRelation left right
  = BinaryRelation
    { _leftToRight :: SM.SetMultimap left right
    , _rightToLeft :: SM.SetMultimap right left
    }

-- | An empty relation.
empty :: BinaryRelation left right
empty = BinaryRelation SM.empty SM.empty

-- | Swaps the relation.
swap
  :: BinaryRelation left right
  -> BinaryRelation right left
swap = BinaryRelation <$> _rightToLeft <*> _leftToRight

-- | Inserts a relation between two values.
insert
  :: ( Ord left, Ord right )
  => left
  -> right
  -> BinaryRelation left right
  -> BinaryRelation left right
insert l r br =
  BinaryRelation
  { _leftToRight = SM.insert l r $ _leftToRight br
  , _rightToLeft = SM.insert r l $ _rightToLeft br
  }

-- | Deletes the relation between to values.
delete
  :: ( Ord left, Ord right )
  => left
  -> right
  -> BinaryRelation left right
  -> BinaryRelation left right
delete l r br =
  BinaryRelation
  { _leftToRight = SM.deleteWithValue l r $ _leftToRight br
  , _rightToLeft = SM.deleteWithValue r l $ _rightToLeft br
  }

-- | Deletes all relations with the given value on the left.
deleteLeft
  :: ( Ord left, Ord right )
  => left
  -> BinaryRelation left right
  -> BinaryRelation left right
deleteLeft l br =
  let rs = S.toList $ byLeft l br
  in
    BinaryRelation
    { _leftToRight = SM.delete l $ _leftToRight br
    , _rightToLeft = foldl' (flip SM.delete) (_rightToLeft br) rs
    }

-- | Deletes all relations with the given value on the right.
deleteRight
  :: ( Ord left, Ord right )
  => right
  -> BinaryRelation left right
  -> BinaryRelation left right
deleteRight r = swap . deleteLeft r . swap

-- | Checks whether the two values are related.
check
  :: ( Ord left, Ord right )
  => left
  -> right
  -> BinaryRelation left right
  -> Bool
check l r = not . S.member r . SM.lookup l . _leftToRight

-- | Gets all values related to the left value.
byLeft
  :: ( Ord left )
  => left
  -> BinaryRelation left right
  -> S.Set right
byLeft l = SM.lookup l . _leftToRight

-- | Gets all values related to the right value.
byRight
  :: ( Ord right )
  => right
  -> BinaryRelation left right
  -> S.Set left
byRight r = SM.lookup r . _rightToLeft

-- | Gets the set of left values satisfying the relation.
lefts
  :: ( Ord left, Ord right )
  => BinaryRelation left right
  -> S.Set left
lefts = SM.keysSet . _leftToRight

-- | Gets the set of right values satisfying the relation.
rights
  :: ( Ord left, Ord right )
  => BinaryRelation left right
  -> S.Set right
rights = SM.keysSet . _rightToLeft

-- | Gets the set of pairs satisfying the relation.
toSet
  :: ( Ord left, Ord right )
  => BinaryRelation left right
  -> S.Set (left, right)
toSet = S.fromList . SM.assocs . _leftToRight

fromList
  :: ( Ord left, Ord right )
  => [(left, right)]
  -> BinaryRelation left right
fromList pairs =
  BinaryRelation
  { _leftToRight = SM.fromList pairs
  , _rightToLeft = SM.fromList (Tuple.swap <$> pairs)
  }
