{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module AlchemyData
  ( IngredientName (IngredientName)
  , EffectName (EffectName)
  , AlchemyData

  -- * Construction
  , emptyAlchemyData
  , learnIngredientEffect
  , learnOverlap

  -- * Queries
  , allKnownIngredients
  , allKnownEffects
  , allKnownOverlaps
  , effectsOf, effectsOfIngredientIn
  , nonEffectsOf, nonEffectsOfIngredientIn
  , ingredientsNotOverlappingWith
  , overlapBetween
  , InconsistentOverlap
  ) where


import           Control.Lens
    ( Ixed (ix)
    , TraversableWithIndex (itraversed)
    , asIndex
    , filtered
    , makeFields
    , toListOf
    , view
    , (&)
    , (^.)
    )
import           Control.Monad
    ( forM_, unless, when )
import           Control.Monad.Trans.State
    ( execState, modify )
import           Data.Coerce
    ( coerce )
import qualified Data.Map.Strict           as M
import           Data.Maybe
    ( isNothing )
import qualified Data.Set                  as S
import qualified PairMap                   as PM


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
      :: PM.PairMap IngredientName (S.Set EffectName) }
$( makeFields ''AlchemyData )


-- | An initial 'AlchemyData' that contains no information about
-- effects or ingredients.
emptyAlchemyData :: AlchemyData
emptyAlchemyData = AlchemyData M.empty M.empty M.empty PM.empty


-- | Gets the set of all known ingredients.
allKnownIngredients :: AlchemyData -> S.Set IngredientName
allKnownIngredients alchemyData =
  S.fromList $ M.keys $ alchemyData^.allIngredients

-- | Gets the set of all known effects.
allKnownEffects :: AlchemyData -> S.Set EffectName
allKnownEffects alchemyData =
  S.fromList $ M.keys $ alchemyData^.positives

-- | Gets all known overlaps between ingredients.
allKnownOverlaps
  :: AlchemyData
  -> [((IngredientName, IngredientName), S.Set EffectName)]
allKnownOverlaps alchemyData = PM.assocs (alchemyData^.fullOverlap)

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
  . filtered (S.member ingName)
  . asIndex )

nonEffectsOfIngredientIn :: AlchemyData -> IngredientName -> S.Set EffectName
nonEffectsOfIngredientIn = flip nonEffectsOf


-- | Gets all ingredients known to not overlap with the given
-- ingredient.
ingredientsNotOverlappingWith
  :: IngredientName
  -> AlchemyData
  -> S.Set IngredientName
ingredientsNotOverlappingWith ingName alchemyData =
  PM.lookup ingName (alchemyData^.fullOverlap) &
  M.filter S.null &
  M.keys &
  S.fromList

-- | Gets the full overlap between the two ingredients if it is known.
overlapBetween
  :: IngredientName
  -> IngredientName
  -> AlchemyData
  -> Maybe (S.Set EffectName)
overlapBetween ing1 ing2 = PM.lookupPair ing1 ing2 . view fullOverlap


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
      forM_ (M.assocs $ PM.lookup ingName (alchemyData^.fullOverlap)) $
        \(otherIng, effs) ->
          when (S.notMember effName effs) $
            modify $ multiMapInsert effName otherIng

    -- The ingredient overlaps map has to be updated because this
    -- operation can contradict old information. We have to preserve
    -- the property that if an effect is contained in two ingredients
    -- that have overlap information, then the effect is contained in
    -- the overlap.
    fullOverlap' = PM.adjust fixOverlap ingName (alchemyData^.fullOverlap)
    fixOverlap = M.mapWithKey $ \otherIng ->
      if S.member otherIng (alchemyData^.positives.ix effName)
      then S.insert effName
      else id


newtype InconsistentOverlap = InconsistentOverlapReason String
instance Show InconsistentOverlap where
  show = coerce


learnOverlap
  :: IngredientName
  -> IngredientName
  -> S.Set EffectName
  -> AlchemyData
  -> Either InconsistentOverlap AlchemyData
learnOverlap ing1 ing2 effs alchemyData =
  do
    checkConsistent
    return $ AlchemyData p' n' all' fullOverlap'
  where
    -- Avoid contradicting pre-existing information.
    --
    -- The overlap is "consistent" if and only if it includes all
    -- effects common to both ingredients and does not include any
    -- effects that either of the ingredients is known to not have,
    -- and the overlap between the ingredients is not already known.
    --
    -- If the overlap is consistent, then the updates are simple: add
    -- both ingredients to the positives map of each effect, add each
    -- ingredient to the negatives map of each effect on the other
    -- ingredient that isn't in the overlap, and insert the new
    -- overlap. Learn the ingredients if they're not already known.
    --
    -- If the overlap is inconsistent, then some of these are true:
    --
    -- - It doesn't have an effect that's common to both ingredients.
    -- - It has an effect that's known to not exist on some ingredient.
    -- - The overlap is already known and it's different.
    --
    -- The easiest thing to do is to just reject it. I could output a
    -- reason too.

    overlapDoesNotAlreadyExist =
      isNothing $ PM.lookupPair ing1 ing2 $ alchemyData^.fullOverlap

    commonEffects = S.intersection
      (alchemyData^.allIngredients.ix ing1)
      (alchemyData^.allIngredients.ix ing2)

    isImpossible eff =
      let impossibleIngs = alchemyData^.negatives.ix eff
      in S.member ing1 impossibleIngs || S.member ing2 impossibleIngs

    throwInconsistent = Left . InconsistentOverlapReason

    overlapIncludesCommonEffects = commonEffects `S.isSubsetOf` effs
    checkOverlapExcludesImpossibleEffects =
      forM_ effs $ \eff ->
        when (isImpossible eff) $
          throwInconsistent $
            "Overlap includes effect " ++
            show eff ++
            " that's known to not exist on one of the ingredients."

    checkConsistent = do
      unless overlapDoesNotAlreadyExist $
        throwInconsistent "Overlap already known"
      unless overlapIncludesCommonEffects $
        throwInconsistent $
          "Overlap doesn't include effects common to both ingredients: " ++
          show (S.toList commonEffects)
      checkOverlapExcludesImpossibleEffects


    ----------------------------------------------------------------------------
    -- The following assumes that the given overlap is "consistent"
    ----------------------------------------------------------------------------

    -- For every effect, add both ingredients to its positive map
    p' = (`execState` (alchemyData^.positives)) $
      forM_ effs $ \eff ->
        modify $
            multiMapInsert eff ing1
          . multiMapInsert eff ing2

    -- For every effect that is on one ingredient but not in the
    -- overlap, add the other ingredient to its negative map
    n' = (`execState` (alchemyData^.negatives)) $ do
      forM_ (alchemyData^.allIngredients.ix ing1) $ \eff1 ->
        when (S.notMember eff1 effs) $
          modify $ multiMapInsert eff1 ing2

      forM_ (alchemyData^.allIngredients.ix ing2) $ \eff2 ->
        when (S.notMember eff2 effs) $
          modify $ multiMapInsert eff2 ing1

    -- Add the effects to both ingredients
    all' = (`execState` (alchemyData^.allIngredients)) $
      if not $ S.null effs
      then
        forM_ effs $ \eff ->
          modify $
              multiMapInsert ing1 eff
            . multiMapInsert ing2 eff
      else do
        -- Ensure at least empty sets exist for the ingredients in
        -- case the overlap is empty
        let insertEmpty = \case
              Nothing -> Just S.empty
              Just s  -> Just s
        modify $
            M.alter insertEmpty ing1
          . M.alter insertEmpty ing2

    -- Set the overlap to the specified one
    fullOverlap' = PM.insertPair ing1 ing2 effs (alchemyData^.fullOverlap)


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
