{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module AlchemyData
  ( IngredientName
  , EffectName
  , AlchemyData

  -- * Construction
  , emptyAlchemyData
  , learnIngredientEffect

  -- * Queries
  , effectsOf, effectsOfIngredientIn
  , nonEffectsOf, nonEffectsOfIngredientIn
  ) where


import           Control.Lens
    ( Ixed (ix)
    , TraversableWithIndex (itraversed)
    , asIndex
    , filtered
    , makeFields
    , to
    , toListOf
    , view
    , (^.)
    )
import           Control.Monad
    ( forM_, when )
import           Control.Monad.Trans.State
    ( execState, modify )
import           Data.Coerce
    ( coerce )
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as S


newtype IngredientName
  = IngredientName String
  deriving ( Eq, Ord )

newtype EffectName
  = EffectName String
  deriving ( Eq, Ord )


-- Let these names print like normal strings
instance Show IngredientName where
  show = coerce
instance Show EffectName where
  show = coerce




data AlchemyData
  = AlchemyData
    { _alchemyDataPositives
      :: M.Map EffectName (S.Set IngredientName)

    , _alchemyDataNegatives
      :: M.Map EffectName (S.Set IngredientName)

    , _alchemyDataAllIngredients
      :: M.Map IngredientName (S.Set EffectName)

    , _alchemyDataFullOverlap
      :: M.Map IngredientName (M.Map IngredientName (S.Set EffectName)) }
$( makeFields ''AlchemyData )


-- | An initial 'AlchemyData' that contains no information about
-- effects or ingredients.
emptyAlchemyData :: AlchemyData
emptyAlchemyData = AlchemyData M.empty M.empty M.empty M.empty



-- | Gets the known effects of the ingredient.
effectsOf :: IngredientName -> AlchemyData -> S.Set EffectName
effectsOf ingName = view $ allIngredients.ix ingName

effectsOfIngredientIn :: AlchemyData -> IngredientName -> S.Set EffectName
effectsOfIngredientIn = flip effectsOf


-- | Gets the known non-effects of the ingredient.
nonEffectsOf :: IngredientName -> AlchemyData -> S.Set EffectName
nonEffectsOf ingName = S.fromList . toListOf
  ( negatives
  . itraversed
  . filtered (S.notMember ingName)
  . asIndex )

nonEffectsOfIngredientIn :: AlchemyData -> IngredientName -> S.Set EffectName
nonEffectsOfIngredientIn = flip nonEffectsOf


-- | Associates the effect to the ingredient.
learnIngredientEffect
  :: IngredientName
  -> EffectName
  -> AlchemyData
  -> AlchemyData
learnIngredientEffect ingName effName alchemyData =
  AlchemyData p' n' all' fullOverlap'
  where
    p' = multiMapInsert effName ingName (alchemyData^.positives)

    all' = multiMapInsert ingName effName (alchemyData^.allIngredients)

    n' = (`execState` (alchemyData^.negatives)) $ do
      -- The ingredient does not /not/ contain the effect (we have to
      -- do this in case this operation contradicts old information)
      modify $ multiMapRemove effName ingName

      -- Every ingredient whose overlap with this one does not contain
      -- this effect is now known to not contain the effect.
      forM_ (alchemyData^.fullOverlap.ix ingName.to M.assocs) $
        \(otherIng, effs) ->
          when (S.notMember effName effs) $
            modify $ multiMapInsert effName otherIng

    -- The ingredient overlaps map has to be updated because this
    -- operation can contradict old information. We have to preserve
    -- the property that if an effect is contained in two ingredients
    -- that have overlap information, then the effect is contained in
    -- the overlap.
    fullOverlap' = M.adjust fixOverlap ingName (alchemyData^.fullOverlap)
    fixOverlap = M.mapWithKey $ \otherIng ->
      if S.member otherIng (alchemyData^.positives.ix effName)
      then S.insert effName
      else id



--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------


multiMapInsert
  :: ( Ord k, Ord a )
  => k -> a -> M.Map k (S.Set a) -> M.Map k (S.Set a)
multiMapInsert k a = M.insertWith (<>) k (S.singleton a)


multiMapRemove
  :: ( Ord k, Ord a )
  => k -> a -> M.Map k (S.Set a) -> M.Map k (S.Set a)
multiMapRemove k a = M.update f k
  where
    f s =
      let s' = S.delete a s
      in if S.null s'
         then Nothing
         else Just s'
