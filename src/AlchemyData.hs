{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

module AlchemyData
  ( IngredientName, ingredientName
  , EffectName, effectName
  , AlchemyData

  -- * Construction
  , emptyAlchemyData
  , learnIngredient
  , learnIngredientEffect
  , learnOverlap

  -- * Queries
  , allKnownIngredients
  , allCompletedIngredients
  , allKnownEffects
  , allKnownOverlaps
  , effectsOf, effectsOfIngredientIn
  , nonEffectsOf, nonEffectsOfIngredientIn
  , ingredientsNotOverlappingWith
  , ingredientsNotContaining
  , isCompleted
  , overlapBetween
  , ingredientsWith
  , ingredientsWithAnyOf

  , InconsistentEffect
  , InconsistentOverlap
  ) where


import           Control.Carrier.State.Strict
    ( execState, get, gets, modify, put, run )
import           Control.Carrier.Throw.Either
    ( runThrow, throwError )
import           Control.Effect.Lens
    ( (%=) )
import           Control.Lens
    ( Ixed (ix)
    , TraversableWithIndex (itraversed)
    , asIndex
    , at
    , filtered
    , makeFields
    , view
    , (%~)
    , (&)
    , (<&>)
    , (^.)
    , (^..)
    )
import           Control.Monad
    ( forM_, unless, when )
import           Data.Coerce
    ( coerce )
import           Data.Foldable
    ( Foldable (fold) )
import           Data.List
    ( foldl1' )
import qualified Data.Map.Strict              as M
import           Data.Maybe
    ( isJust, isNothing )
import qualified Data.Set                     as S
import qualified Data.Text                    as T
import qualified PairMap                      as PM


newtype IngredientName
  = IngredientName T.Text
  deriving ( Eq, Ord )

newtype EffectName
  = EffectName T.Text
  deriving ( Eq, Ord )


-- Let these names print in title case
instance Show IngredientName where
  show = T.unpack . T.toTitle . coerce
instance Show EffectName where
  show = T.unpack . T.toTitle . coerce


-- Ignore case in names
ingredientName :: T.Text -> IngredientName
ingredientName = IngredientName . T.toLower

effectName :: T.Text -> EffectName
effectName = EffectName . T.toLower


data AlchemyData
  = AlchemyData
    { _alchemyDataPositives
      :: M.Map EffectName (S.Set IngredientName)
    -- ^ Multimap from effects to ingredients containing them.

    , _alchemyDataNegativesNonComplete
      :: M.Map EffectName (S.Set IngredientName)
    -- ^ Multimap from effects to incomplete ingredients known to not
    -- contain them.

    , _alchemyDataAllIngredients
      :: M.Map IngredientName (S.Set EffectName)
    -- ^ Map from ingredients to their known effects.

    , _alchemyDataCompletedIngredients
      :: S.Set IngredientName
    -- ^ The set of all ingredients with all 4 effects known.

    , _alchemyDataFullOverlapNonComplete
      :: PM.PairMap IngredientName (S.Set EffectName)
    -- ^ A map from pairs of incomplete ingredients to the set of
    -- ingredients they have in common, as gleaned from actually
    -- combining them.
    }
$( makeFields ''AlchemyData )


completedIngredientsNotContaining
  :: EffectName -> AlchemyData -> S.Set IngredientName
completedIngredientsNotContaining eff alchemyData =
  S.filter doesNotContainEff (alchemyData^.completedIngredients)
  where
    doesNotContainEff ing =
      S.notMember eff (alchemyData^.allIngredients.ix ing)


ingredientsNotContaining :: EffectName -> AlchemyData -> S.Set IngredientName
ingredientsNotContaining eff alchemyData =
  S.union
    (alchemyData^.negativesNonComplete.ix eff)
    (completedIngredientsNotContaining eff alchemyData)


isCompleted :: IngredientName -> AlchemyData -> Bool
isCompleted ing alchemyData =
  S.member ing (alchemyData^.completedIngredients)


-- | Gets the full overlap between the two ingredients if it is known.
overlapBetween
  :: IngredientName
  -> IngredientName
  -> AlchemyData
  -> Maybe (S.Set EffectName)
overlapBetween ing1 ing2 alchemyData
  | isCompleted1 = overlapWithCompleted ing1 ing2
  | isCompleted2 = overlapWithCompleted ing2 ing1
  | otherwise = PM.lookupPair ing1 ing2 (alchemyData^.fullOverlapNonComplete)
  where
    isCompleted1 = isCompleted ing1 alchemyData
    isCompleted2 = isCompleted ing2 alchemyData

    overlapWithCompleted completedIng otherIng =
      let completedEffs = effectsOf completedIng alchemyData
          positiveEffs = S.filter otherIngContains completedEffs
          negativeEffs = S.filter otherIngNotContains completedEffs

          otherIngContains eff =
            S.member otherIng (alchemyData^.positives.ix eff)
          otherIngNotContains eff =
            S.member otherIng (ingredientsNotContaining eff alchemyData)

          everyEffKnown =
            S.size positiveEffs + S.size negativeEffs == S.size completedEffs
      in if everyEffKnown
         then Just positiveEffs
         else Nothing


-- | An initial 'AlchemyData' that contains no information about
-- effects or ingredients.
emptyAlchemyData :: AlchemyData
emptyAlchemyData = AlchemyData M.empty M.empty M.empty S.empty PM.empty


allCompletedIngredients :: AlchemyData -> S.Set IngredientName
allCompletedIngredients = view completedIngredients

-- | Gets the set of all known ingredients.
allKnownIngredients :: AlchemyData -> S.Set IngredientName
allKnownIngredients alchemyData =
  S.fromList $ M.keys $ alchemyData^.allIngredients

-- | Gets the set of all known effects.
allKnownEffects :: AlchemyData -> S.Set EffectName
allKnownEffects alchemyData =
  S.fromList $ M.keys $ alchemyData^.positives

-- | Gets all known overlaps between pairs of ingredients.
allKnownOverlaps
  :: AlchemyData
  -> [((IngredientName, IngredientName), S.Set EffectName)]
allKnownOverlaps alchemyData =
  PM.assocs (alchemyData^.fullOverlapNonComplete) ++
  [ ((ing1, ing2), overlap)
  | ing1 <- S.toList (alchemyData^.completedIngredients)
  , ing2 <- M.keys (alchemyData^.allIngredients)
  , ing1 /= ing2
  , let maybeOverlap = overlapBetween ing1 ing2 alchemyData
        Just overlap = maybeOverlap
  , isJust maybeOverlap ]

-- | Gets the known effects of the ingredient.
effectsOf :: IngredientName -> AlchemyData -> S.Set EffectName
effectsOf ingName = view $ allIngredients.ix ingName

effectsOfIngredientIn :: AlchemyData -> IngredientName -> S.Set EffectName
effectsOfIngredientIn = flip effectsOf


-- | Gets the known non-effects of the ingredient.
nonEffectsOf :: IngredientName -> AlchemyData -> S.Set EffectName
nonEffectsOf ing alchemyData
  | isIngCompleted = S.difference allEffs ingEffs
  | otherwise      = effsNotContainingNonComplete
  where
    isIngCompleted = isCompleted ing alchemyData
    allEffs = allKnownEffects alchemyData
    ingEffs = effectsOf ing alchemyData

    effsNotContainingNonComplete = S.fromList $
      alchemyData^..negativesNonComplete
      . itraversed
      . filtered (S.member ing)
      . asIndex

nonEffectsOfIngredientIn :: AlchemyData -> IngredientName -> S.Set EffectName
nonEffectsOfIngredientIn = flip nonEffectsOf


ingredientsWith :: EffectName -> AlchemyData -> S.Set IngredientName
ingredientsWith = ingredientsWithAnyOf . S.singleton

ingredientsWithAnyOf :: S.Set EffectName -> AlchemyData -> S.Set IngredientName
ingredientsWithAnyOf effs alchemyData =
  S.unions (S.toList effs <&> \eff -> alchemyData^.positives.ix eff)


-- | Gets all ingredients known to not overlap with the given
-- ingredient.
ingredientsNotOverlappingWith
  :: IngredientName
  -> AlchemyData
  -> S.Set IngredientName
ingredientsNotOverlappingWith ing alchemyData
  | isIngCompleted = ingsNotContainingIngEffs
  | otherwise      = ingsNotOverlappingIncompleteIng
  where
    isIngCompleted = isCompleted ing alchemyData

    ingsNotContainingIngEffs =
      foldl1' S.intersection
      [ ingredientsNotContaining eff alchemyData
      | eff <- S.toList $ effectsOf ing alchemyData ]

    ingsNotOverlappingIncompleteIng =
      S.union
        completedIngsNotOverlappingIng
        incompleteIngsNotOverlappingIng

    completedIngsNotOverlappingIng =
      S.filter
        (\other -> S.null $ fold $ overlapBetween ing other alchemyData)
        (allCompletedIngredients alchemyData)

    incompleteIngsNotOverlappingIng =
       PM.lookup ing (alchemyData^.fullOverlapNonComplete) &
       M.filter S.null &
       M.keys &
       S.fromList


-- | Acknowledges the existence of the ingredient.
learnIngredient
  :: IngredientName
  -> AlchemyData
  -> AlchemyData
learnIngredient ingName alchemyData =
  alchemyData & allIngredients.at ingName %~ \case
    Nothing -> Just S.empty
    Just s  -> Just s


newtype InconsistentEffect = InconsistentEffectReason T.Text
instance Show InconsistentEffect where
  show = T.unpack . coerce


-- | Associates the effect to the ingredient.
learnIngredientEffect
  :: IngredientName
  -> EffectName
  -> AlchemyData
  -> Either InconsistentEffect AlchemyData
learnIngredientEffect ing eff alchemyData =
  do
    checkConsistent
    return newAlchemyData
  where

    checkConsistent = do
      checkNotAlreadyComplete
      checkNotKnownToNotContainEff

    throwInconsistent = Left . InconsistentEffectReason

    checkNotAlreadyComplete =
      when (isCompleted ing alchemyData) $
        throwInconsistent "Ingredient is already completed"

    checkNotKnownToNotContainEff =
      when (S.member ing $ ingredientsNotContaining eff alchemyData) $
        throwInconsistent
          "Ingredient was believed to not contain the effect"

    newIngEffs = S.insert eff $ effectsOf ing alchemyData

    isNewlyComplete = S.size newIngEffs == 4
    newAlchemyData
      | isNewlyComplete = newlyCompleteIngAlchemyData
      | otherwise       = updatedIngAlchemyData

    -- Ingredient is now known to contain the effect
    p'   = multiMapInsert eff ing (alchemyData^.positives)
    all' = multiMapInsert ing eff (alchemyData^.allIngredients)

    -- Every incomplete ingredient whose overlap with ing does not
    -- contain eff is now known to not contain eff
    n' = run . execState (alchemyData^.negativesNonComplete) $ do
      forM_ (M.assocs $
              PM.lookup ing (alchemyData^.fullOverlapNonComplete)) $
        \(otherIng, overlap) ->
          when (S.notMember eff overlap) $
            modify $ multiMapInsert eff otherIng

    newlyCompleteIngAlchemyData =
      let
        completed' = S.insert ing (alchemyData^.completedIngredients)

        -- Remove all pairs containing ing since it is completed
        overlap' = PM.delete ing (alchemyData^.fullOverlapNonComplete)
      in AlchemyData p' n' all' completed' overlap'

    updatedIngAlchemyData =
      let
        -- No new completed ingredient
        completed' = alchemyData^.completedIngredients
        -- No new overlap information
        overlap' = alchemyData^.fullOverlapNonComplete
      in AlchemyData p' n' all' completed' overlap'


data InconsistentOverlap
  = InconsistentOverlapReason T.Text
  | InconsistentOverlapBadEffects InconsistentEffect
instance Show InconsistentOverlap where
  show = \case
    InconsistentOverlapReason t     -> T.unpack t
    InconsistentOverlapBadEffects e -> show e



learnOverlap
  :: IngredientName
  -> IngredientName
  -> S.Set EffectName
  -> AlchemyData
  -> Either InconsistentOverlap AlchemyData
learnOverlap ing1 ing2 effs alchemyData =
  do
    checkConsistent
    run .
      runThrow @InconsistentOverlap .
      execState alchemyData $
      update
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
      isNothing $ overlapBetween ing1 ing2 alchemyData

    commonEffects = S.intersection
      (alchemyData^.allIngredients.ix ing1)
      (alchemyData^.allIngredients.ix ing2)

    isImpossible eff =
      let impossibleIngs = ingredientsNotContaining eff alchemyData
      in S.member ing1 impossibleIngs || S.member ing2 impossibleIngs

    throwInconsistent = Left . InconsistentOverlapReason

    overlapIncludesCommonEffects = commonEffects `S.isSubsetOf` effs
    checkOverlapExcludesImpossibleEffects =
      forM_ effs $ \eff ->
        when (isImpossible eff) $
          throwInconsistent $
            "Overlap includes effect " <>
            T.pack (show eff) <>
            " that's known to not exist on one of the ingredients."

    checkConsistent = do
      unless overlapDoesNotAlreadyExist $
        throwInconsistent "Overlap already known"
      unless overlapIncludesCommonEffects $
        throwInconsistent $
          "Overlap doesn't include effects common to both ingredients: " <>
          T.pack (show $ S.toList commonEffects)
      checkOverlapExcludesImpossibleEffects


    ----------------------------------------------------------------------------
    -- The following assumes that the given overlap is "consistent"
    ----------------------------------------------------------------------------

    update = do
      let liftLearnIngredientEffect i e = do
            a <- get
            case learnIngredientEffect i e a of
              Left err -> throwError $ InconsistentOverlapBadEffects err
              Right a' -> put a'

      -- Learn the ingredients
      modify $
        learnIngredient ing1 .
        learnIngredient ing2

      -- Learn the effects on both ingredients
      mapM_ (liftLearnIngredientEffect ing1) effs
      mapM_ (liftLearnIngredientEffect ing2) effs

      isCompleted1 <- gets (isCompleted ing1)
      isCompleted2 <- gets (isCompleted ing2)

      -- Update the effect negatives for noncompleted ingredients
      unless isCompleted1 $ updateNegatives ing1 ing2
      unless isCompleted2 $ updateNegatives ing2 ing1

      -- Update overlap map if both ingredients are not completed
      when (not isCompleted1 && not isCompleted2) $
        fullOverlapNonComplete @AlchemyData %= PM.insertPair ing1 ing2 effs


    updateNegatives incompleteIng otherIng = do
      otherIngEffs <- gets $ effectsOf otherIng
      forM_ otherIngEffs $ \eff -> do
        when (S.notMember eff effs) $
          negativesNonComplete @AlchemyData %= multiMapInsert eff incompleteIng

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------


multiMapInsert
  :: ( Ord k, Ord a )
  => k -> a -> M.Map k (S.Set a) -> M.Map k (S.Set a)
multiMapInsert k a = M.insertWith (<>) k (S.singleton a)
