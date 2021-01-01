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
  , ingredientsNotContaining
  , ingredientsNotOverlappingWith
  , ingredientsOverlappingWith
  , overlapBetween
  , isCompleted
  , ingredientsWith
  , ingredientsWithAnyOf

  , InconsistentEffect
  , InconsistentOverlap
  ) where


import qualified BinaryRelation               as BR
import           Control.Algebra
    ( Has )
import           Control.Carrier.Error.Either
    ( throwError )
import           Control.Carrier.Error.Extra
    ( rethrowing )
import           Control.Carrier.State.Strict
    ( gets )
import           Control.Carrier.Throw.Either
    ( runThrow )
import           Control.Effect.Error
    ( Error )
import           Control.Effect.Lens
    ( (%=) )
import           Control.Effect.State
    ( State )
import           Control.Lens
    ( makeLenses, view, (&), (<&>), (^.) )
import           Control.Monad
    ( forM_, unless, when )
import           Control.Monad.Extra
    ( whenM )
import           Data.Coerce
    ( coerce )
import           Data.Either.Extra
    ( fromEither )
import           Data.Foldable
    ( Foldable (fold) )
import           Data.List
    ( foldl1' )
import qualified Data.Map.Strict              as M
import           Data.Maybe
    ( isJust )
import qualified Data.Set                     as S
import qualified Data.Text                    as T
import           Data.UPair
    ( UPair, distinctPairs, pair, unpair )
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
    { _ingHasEffRelation
      :: BR.BinaryRelation IngredientName EffectName
    -- ^ The "ingredient has effect" relation between ingredients and
    -- effects.

    , _incompleteIngNotHasEffRelation
      :: BR.BinaryRelation IngredientName EffectName
    -- ^ The "ingredient does not have effect" relation between
    -- _incomplete_ ingredients and effects.

    , _completedIngredients
      :: S.Set IngredientName
    -- ^ The set of all ingredients with all 4 effects known.

    , _emptyIngredients
      :: S.Set IngredientName
    -- ^ The set of ingredients for which no effects are known.
    --
    -- Despite not knowing any of an ingredient's effects, we might
    -- still have information about the ingredient in the form of
    -- empty overlaps with other ingredients.

    , _fullOverlapNonComplete
      :: PM.PairMap IngredientName (S.Set EffectName)
    -- ^ A map from pairs of incomplete ingredients to the set of
    -- ingredients they have in common, as gleaned from actually
    -- combining them.
    }
$( makeLenses ''AlchemyData )


-- | An initial 'AlchemyData' that contains no information about
-- effects or ingredients.
emptyAlchemyData :: AlchemyData
emptyAlchemyData = AlchemyData BR.empty BR.empty S.empty S.empty PM.empty

-- | Whether the ingredient is completed (i.e. has all 4 effects).
isCompleted :: IngredientName -> AlchemyData -> Bool
isCompleted ing alchemyData =
  S.member ing (alchemyData^.completedIngredients)

-- | Gets the set of all completed ingredients.
allCompletedIngredients :: AlchemyData -> S.Set IngredientName
allCompletedIngredients = view completedIngredients

-- | Gets the set of all incomplete ingredients.
allIncompleteIngredients :: AlchemyData -> S.Set IngredientName
allIncompleteIngredients alchemyData =
  allKnownIngredients alchemyData
  `S.difference` allCompletedIngredients alchemyData

-- | Gets the set of all known ingredients.
allKnownIngredients :: AlchemyData -> S.Set IngredientName
allKnownIngredients alchemyData =
  S.union
    (BR.lefts $ alchemyData^.ingHasEffRelation)
    (alchemyData^.emptyIngredients)

-- | Gets the set of all known effects.
allKnownEffects :: AlchemyData -> S.Set EffectName
allKnownEffects = BR.rights . view ingHasEffRelation

-- | Gets all known overlaps between pairs of ingredients.
allKnownOverlaps
  :: AlchemyData
  -> [(UPair IngredientName, S.Set EffectName)]
allKnownOverlaps alchemyData =
  PM.assocs (alchemyData^.fullOverlapNonComplete) ++

  -- Overlaps between completed and incomplete ingredients
  [ (pair ing1 ing2, overlap)
  | ing1 <- S.toList (allCompletedIngredients alchemyData)
  , ing2 <- S.toList (allIncompleteIngredients alchemyData)
  , let maybeOverlap = overlapBetween ing1 ing2 alchemyData
        Just overlap = maybeOverlap
  , isJust maybeOverlap ] ++

  -- Overlaps between pairs of completed ingredients
  [ (ings, overlap)
  | ings <- distinctPairs (allCompletedIngredients alchemyData)
  , let (ing1, ing2) = unpair ings
        maybeOverlap = overlapBetween ing1 ing2 alchemyData
        Just overlap = maybeOverlap
  , isJust maybeOverlap ]

-- | Gets the known effects of the ingredient.
effectsOf :: IngredientName -> AlchemyData -> S.Set EffectName
effectsOf ing = BR.byLeft ing . view ingHasEffRelation

effectsOfIngredientIn :: AlchemyData -> IngredientName -> S.Set EffectName
effectsOfIngredientIn = flip effectsOf

-- | Returns whether an ingredient is known to not have an effect.
doesNotHave :: IngredientName -> EffectName -> AlchemyData -> Bool
doesNotHave ing eff alchemyData
  | isCompleted ing alchemyData = S.notMember eff $ effectsOf ing alchemyData
  | otherwise                   = useRelation
  where
    useRelation = BR.check ing eff $ alchemyData^.incompleteIngNotHasEffRelation

-- | Returns whether an ingredient is known to have an effect.
hasEffect :: IngredientName -> EffectName -> AlchemyData -> Bool
hasEffect ing eff = S.member eff . effectsOf ing

-- | Gets the known non-effects of the ingredient.
nonEffectsOf :: IngredientName -> AlchemyData -> S.Set EffectName
nonEffectsOf ing alchemyData
  | isIngCompleted = S.difference allEffs ingEffs
  | otherwise      = effsNotContainingNonComplete
  where
    isIngCompleted = isCompleted ing alchemyData
    allEffs = allKnownEffects alchemyData
    ingEffs = effectsOf ing alchemyData

    effsNotContainingNonComplete =
      BR.byLeft ing $ alchemyData^.incompleteIngNotHasEffRelation

nonEffectsOfIngredientIn :: AlchemyData -> IngredientName -> S.Set EffectName
nonEffectsOfIngredientIn = flip nonEffectsOf


-- | Gets the set of ingredients known to not contain an effect.
ingredientsNotContaining :: EffectName -> AlchemyData -> S.Set IngredientName
ingredientsNotContaining eff alchemyData =
  S.union
    incompleteIngsNotContainingEff
    completeIngsNotContainingEff
  where
    incompleteIngsNotContainingEff =
      BR.byRight eff $ alchemyData^.incompleteIngNotHasEffRelation
    completeIngsNotContainingEff =
      flip S.filter (alchemyData^.completedIngredients) $ \ing ->
        not (BR.check ing eff $ alchemyData^.ingHasEffRelation)

-- | Gets the set of ingredients known to contain an effect.
ingredientsWith :: EffectName -> AlchemyData -> S.Set IngredientName
ingredientsWith eff = BR.byRight eff . view ingHasEffRelation

ingredientsWithEffectIn :: AlchemyData -> EffectName -> S.Set IngredientName
ingredientsWithEffectIn = flip ingredientsWith

-- | Gets the set of ingredients known to contain any of the effects.
ingredientsWithAnyOf :: S.Set EffectName -> AlchemyData -> S.Set IngredientName
ingredientsWithAnyOf effs alchemyData =
  S.unions (S.toList effs <&> \eff ->
               ingredientsWith eff alchemyData)


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

-- | Gets all ingredients that overlap with the given ingredient.
ingredientsOverlappingWith
  :: IngredientName
  -> AlchemyData
  -> S.Set IngredientName
ingredientsOverlappingWith ing alchemyData =
  S.unions (S.map (ingredientsWithEffectIn alchemyData) $
            effectsOf ing alchemyData)

-- | Gets the full overlap between the two ingredients if it is known.
overlapBetween
  :: IngredientName
  -> IngredientName
  -> AlchemyData
  -> Maybe (S.Set EffectName)
overlapBetween ing1 ing2 alchemyData
  | isCompleted1 = overlapWithCompleted ing1 ing2
  | isCompleted2 = overlapWithCompleted ing2 ing1
  | otherwise = PM.lookupPair (pair ing1 ing2) (alchemyData^.fullOverlapNonComplete)
  where
    isCompleted1 = isCompleted ing1 alchemyData
    isCompleted2 = isCompleted ing2 alchemyData

    overlapWithCompleted completedIng otherIng =
      let completedEffs = effectsOf completedIng alchemyData
          positiveEffs = S.filter otherIngContains completedEffs
          negativeEffs = S.filter otherIngNotContains completedEffs

          otherIngContains eff = (otherIng `hasEffect` eff) alchemyData
          otherIngNotContains eff = (otherIng `doesNotHave` eff) alchemyData

          everyEffKnown =
            S.union positiveEffs negativeEffs == completedEffs
      in if everyEffKnown
         then Just positiveEffs
         else Nothing


-- | Acknowledges the existence of the ingredient.
learnIngredient
  :: ( Has (State AlchemyData) sig m )
  => IngredientName
  -> m ()
learnIngredient ing =
  whenM isIngredientUnknown $
    emptyIngredients %= S.insert ing
  where
    isIngredientUnknown = gets $ S.null . effectsOf ing


newtype InconsistentEffect = InconsistentEffectReason T.Text
instance Show InconsistentEffect where
  show = T.unpack . coerce


-- | Associates the effect to the ingredient.
learnIngredientEffect
  :: ( Has (State AlchemyData) sig m
     , Has (Error InconsistentEffect) sig m
     )
  => IngredientName
  -> EffectName
  -> m ()
learnIngredientEffect ing eff = fmap fromEither . runThrow @() $
  do
    let exitEarly = throwError ()

    -- Do nothing if the ingredient already has the effect
    whenM (gets $ ing `hasEffect` eff)
      exitEarly

    ----------------------------------------------------------------------------
    -- Avoid contradicting old information
    ----------------------------------------------------------------------------

    -- Check ingredient isn't already complete
    whenM (gets $ isCompleted ing) $
      throwError $ InconsistentEffectReason
        "Ingredient is already completed"

    -- Check ingredient isn't known to /not/ contain the effect
    whenM (gets $ S.member ing . ingredientsNotContaining eff) $
      throwError $ InconsistentEffectReason
        "Ingredient was believed to not contain the effect"

    ----------------------------------------------------------------------------
    -- Update negatives for incomplete ings overlapping with ing
    ----------------------------------------------------------------------------
    -- Every incomplete ingredient whose overlap with ing does not
    -- contain eff is now known to not contain eff
    ----------------------------------------------------------------------------
    do
      overlapsWithIng <-
        gets $ PM.lookup ing . view fullOverlapNonComplete

      forM_ (M.assocs overlapsWithIng) $ \(otherIng, overlap) ->
        when (S.notMember eff overlap) $
          incompleteIngNotHasEffRelation %= BR.insert otherIng eff


    ----------------------------------------------------------------------------
    -- Update completed & overlap maps if the ingredient became completed
    ----------------------------------------------------------------------------
    isNewlyComplete <- do
      newIngEffs <- gets $ S.insert eff . effectsOf ing
      return $ S.size newIngEffs == 4
    when isNewlyComplete $ do
      -- Add completed ingredient to completed set
      completedIngredients %= S.insert ing

      -- Remove completed ingredient from maps that should not have
      -- completed ingredients
      fullOverlapNonComplete %= PM.delete ing
      incompleteIngNotHasEffRelation %= BR.deleteLeft ing

    ----------------------------------------------------------------------------
    -- Record positive relation between ing and eff
    ----------------------------------------------------------------------------
    ingHasEffRelation %= BR.insert ing eff
    emptyIngredients %= S.delete ing


data InconsistentOverlap
  = InconsistentOverlapReason T.Text
  | InconsistentOverlapBadEffects InconsistentEffect
instance Show InconsistentOverlap where
  show = \case
    InconsistentOverlapReason t     -> T.unpack t
    InconsistentOverlapBadEffects e -> show e



learnOverlap
  :: ( Has (State AlchemyData) sig m
     , Has (Error InconsistentOverlap) sig m
     )
  => IngredientName
  -> IngredientName
  -> S.Set EffectName
  -> m ()
learnOverlap ing1 ing2 effs = fmap fromEither . runThrow @() $
  do
    let exitEarly = throwError ()

    -- Do nothing if this overlap is already known
    whenM ((== Just effs) <$> gets (overlapBetween ing1 ing2))
      exitEarly

    ----------------------------------------------------------------------------
    -- Avoid contradicting pre-existing information
    ----------------------------------------------------------------------------
    -- The overlap is "consistent" if and only if it includes all
    -- effects common to both ingredients and does not include any
    -- effects that either of the ingredients is known to not have,
    -- and the overlap between the ingredients is not already known.
    ----------------------------------------------------------------------------

    -- Ensure overlap doesn't already exist
    unlessNothingM (gets $ overlapBetween ing1 ing2) $ \overlap ->
      throwError $ InconsistentOverlapReason $
        "Overlap between " <>
        T.pack (show ing1) <> " and " <> T.pack (show ing2) <>
        " already known: " <> T.pack (show overlap)

    -- Ensure overlap includes common effects
    do
      effs1 <- gets $ effectsOf ing1
      effs2 <- gets $ effectsOf ing2
      let commonEffects = S.intersection effs1 effs2
      unless (commonEffects `S.isSubsetOf` effs) $ do
        throwError $ InconsistentOverlapReason $
          "Overlap does not include effects common to both ingredients: " <>
          T.pack (show $ S.toList commonEffects)

    -- Ensure overlap excludes impossible effects
    do
      let isImpossible eff = do
            impossibleIngs <- gets $ ingredientsNotContaining eff
            return $
              S.member ing1 impossibleIngs ||
              S.member ing2 impossibleIngs
      forM_ effs $ \eff ->
        whenM (isImpossible eff) $
          throwError $ InconsistentOverlapReason $
            "Overlap includes effect " <>
            T.pack (show eff) <>
            " that's known to not exist on one of the ingredients."

    ----------------------------------------------------------------------------
    -- Learn the new effects and ingredients
    ----------------------------------------------------------------------------

    -- Learn the ingredients
    mapM_ learnIngredient [ing1, ing2]

    -- Learn the effects on both ingredients
    rethrowing InconsistentOverlapBadEffects $
      forM_ effs $ \eff -> do
        learnIngredientEffect ing1 eff
        learnIngredientEffect ing2 eff


    ----------------------------------------------------------------------------
    -- Update negatives and overlap maps
    ----------------------------------------------------------------------------
    let updateNegatives incompleteIng otherIng = do
          otherIngEffs <- gets $ effectsOf otherIng
          forM_ otherIngEffs $ \eff -> do
            when (S.notMember eff effs) $
              incompleteIngNotHasEffRelation %= BR.insert incompleteIng eff

    isCompleted1 <- gets (isCompleted ing1)
    isCompleted2 <- gets (isCompleted ing2)

    -- Update the effect negatives for noncompleted ingredients
    unless isCompleted1 $ updateNegatives ing1 ing2
    unless isCompleted2 $ updateNegatives ing2 ing1

    -- Update overlap map if both ingredients are not completed
    when (not isCompleted1 && not isCompleted2) $
      fullOverlapNonComplete %= PM.insertPair (pair ing1 ing2) effs


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

unlessNothingM
  :: Monad m
  => m (Maybe a)
  -> (a -> m ())
  -> m ()
unlessNothingM action f =
  action >>= \case
    Just a -> f a
    Nothing -> return ()

