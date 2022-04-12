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
  , Overlap (Overlap)

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
import qualified Data.Map.Strict              as Map
import           Data.Maybe
    ( isJust )
import           Data.Set
    ( Set )
import qualified Data.Set                     as Set
import qualified Data.Text                    as T
import           Data.UPair
    ( UPair, distinctPairs, pair, unpair )
import           PairMap
    ( PairMap )
import qualified PairMap                      as PairMap


newtype IngredientName
  = IngredientName T.Text
  deriving ( Eq, Ord )

newtype EffectName
  = EffectName T.Text
  deriving ( Eq, Ord )


data Overlap
  = Overlap IngredientName IngredientName (Set EffectName)
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
      :: Set IngredientName
    -- ^ The set of all ingredients with all 4 effects known.

    , _emptyIngredients
      :: Set IngredientName
    -- ^ The set of ingredients for which no effects are known.
    --
    -- Despite not knowing any of an ingredient's effects, we might
    -- still have information about the ingredient in the form of
    -- empty overlaps with other ingredients.

    , _fullOverlapNonComplete
      :: PairMap IngredientName (Set EffectName)
    -- ^ A map from pairs of incomplete ingredients to the set of
    -- ingredients they have in common, as gleaned from actually
    -- combining them.
    }
$( makeLenses ''AlchemyData )


-- | An initial 'AlchemyData' that contains no information about
-- effects or ingredients.
emptyAlchemyData :: AlchemyData
emptyAlchemyData = AlchemyData BR.empty BR.empty Set.empty Set.empty PairMap.empty

-- | Whether the ingredient is completed (i.e. has all 4 effects).
isCompleted :: IngredientName -> AlchemyData -> Bool
isCompleted ing alchemyData =
  Set.member ing (alchemyData^.completedIngredients)

-- | Gets the set of all completed ingredients.
allCompletedIngredients :: AlchemyData -> Set IngredientName
allCompletedIngredients = view completedIngredients

-- | Gets the set of all incomplete ingredients.
allIncompleteIngredients :: AlchemyData -> Set IngredientName
allIncompleteIngredients alchemyData =
  allKnownIngredients alchemyData
  `Set.difference` allCompletedIngredients alchemyData

-- | Gets the set of all known ingredients.
allKnownIngredients :: AlchemyData -> Set IngredientName
allKnownIngredients alchemyData =
  Set.union
    (BR.lefts $ alchemyData^.ingHasEffRelation)
    (alchemyData^.emptyIngredients)

-- | Gets the set of all known effects.
allKnownEffects :: AlchemyData -> Set EffectName
allKnownEffects = BR.rights . view ingHasEffRelation

-- | Gets all known overlaps between pairs of ingredients.
allKnownOverlaps
  :: AlchemyData
  -> [(UPair IngredientName, Set EffectName)]
allKnownOverlaps alchemyData =
  PairMap.assocs (alchemyData^.fullOverlapNonComplete) ++

  -- Overlaps between completed and incomplete ingredients
  [ (pair ing1 ing2, overlap)
  | ing1 <- Set.toList (allCompletedIngredients alchemyData)
  , ing2 <- Set.toList (allIncompleteIngredients alchemyData)
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
effectsOf :: IngredientName -> AlchemyData -> Set EffectName
effectsOf ing = BR.byLeft ing . view ingHasEffRelation

effectsOfIngredientIn :: AlchemyData -> IngredientName -> Set EffectName
effectsOfIngredientIn = flip effectsOf

-- | Returns whether an ingredient is known to not have an effect.
doesNotHave :: IngredientName -> EffectName -> AlchemyData -> Bool
doesNotHave ing eff alchemyData
  | isCompleted ing alchemyData = not (hasEffect ing eff alchemyData)
  | otherwise                   = useRelation
  where
    useRelation = BR.check ing eff $ alchemyData^.incompleteIngNotHasEffRelation

-- | Returns whether an ingredient is known to have an effect.
hasEffect :: IngredientName -> EffectName -> AlchemyData -> Bool
hasEffect ing eff alchemyData =
  BR.check ing eff (alchemyData^.ingHasEffRelation)

-- | Gets the known non-effects of the ingredient.
nonEffectsOf :: IngredientName -> AlchemyData -> Set EffectName
nonEffectsOf ing alchemyData
  | isIngCompleted = Set.difference allEffs ingEffs
  | otherwise      = effsNotContainingNonComplete
  where
    isIngCompleted = isCompleted ing alchemyData
    allEffs = allKnownEffects alchemyData
    ingEffs = effectsOf ing alchemyData

    effsNotContainingNonComplete =
      BR.byLeft ing $ alchemyData^.incompleteIngNotHasEffRelation

nonEffectsOfIngredientIn :: AlchemyData -> IngredientName -> Set EffectName
nonEffectsOfIngredientIn = flip nonEffectsOf


-- | Gets the set of ingredients known to not contain an effect.
ingredientsNotContaining :: EffectName -> AlchemyData -> Set IngredientName
ingredientsNotContaining eff alchemyData =
  Set.union
    incompleteIngsNotContainingEff
    completeIngsNotContainingEff
  where
    incompleteIngsNotContainingEff =
      BR.byRight eff $ alchemyData^.incompleteIngNotHasEffRelation
    completeIngsNotContainingEff =
      flip Set.filter (alchemyData^.completedIngredients) $ \ing ->
        not (BR.check ing eff $ alchemyData^.ingHasEffRelation)

-- | Gets the set of ingredients known to contain an effect.
ingredientsWith :: EffectName -> AlchemyData -> Set IngredientName
ingredientsWith eff = BR.byRight eff . view ingHasEffRelation

ingredientsWithEffectIn :: AlchemyData -> EffectName -> Set IngredientName
ingredientsWithEffectIn = flip ingredientsWith

-- | Gets the set of ingredients known to contain any of the effects.
ingredientsWithAnyOf :: Set EffectName -> AlchemyData -> Set IngredientName
ingredientsWithAnyOf effs alchemyData =
  Set.unions (Set.toList effs <&> \eff ->
              ingredientsWith eff alchemyData)


-- | Gets all ingredients known to not overlap with the given
-- ingredient.
ingredientsNotOverlappingWith
  :: IngredientName
  -> AlchemyData
  -> Set IngredientName
ingredientsNotOverlappingWith ing alchemyData
  | isIngCompleted = ingsNotContainingIngEffs
  | otherwise      = ingsNotOverlappingIncompleteIng
  where
    isIngCompleted = isCompleted ing alchemyData

    ingsNotContainingIngEffs =
      foldl1' Set.intersection
      [ ingredientsNotContaining eff alchemyData
      | eff <- Set.toList $ effectsOf ing alchemyData ]

    ingsNotOverlappingIncompleteIng =
      Set.union
        completedIngsNotOverlappingIng
        incompleteIngsNotOverlappingIng

    completedIngsNotOverlappingIng =
      Set.filter
        (\other -> Set.null $ fold $ overlapBetween ing other alchemyData)
        (allCompletedIngredients alchemyData)

    incompleteIngsNotOverlappingIng =
       PairMap.lookup ing (alchemyData^.fullOverlapNonComplete) &
       Map.filter Set.null &
       Map.keys &
       Set.fromList

-- | Gets all ingredients that overlap with the given ingredient.
ingredientsOverlappingWith
  :: IngredientName
  -> AlchemyData
  -> Set IngredientName
ingredientsOverlappingWith ing alchemyData =
  Set.unions (Set.map (ingredientsWithEffectIn alchemyData) $
              effectsOf ing alchemyData)

-- | Gets the full overlap between the two ingredients if it is known.
overlapBetween
  :: IngredientName
  -> IngredientName
  -> AlchemyData
  -> Maybe (Set EffectName)
overlapBetween ing1 ing2 alchemyData
  | isCompleted1 = overlapWithCompleted ing1 ing2
  | isCompleted2 = overlapWithCompleted ing2 ing1
  | otherwise = PairMap.lookupPair (pair ing1 ing2)
                                   (alchemyData^.fullOverlapNonComplete)
  where
    isCompleted1 = isCompleted ing1 alchemyData
    isCompleted2 = isCompleted ing2 alchemyData

    overlapWithCompleted completedIng otherIng =
      let completedEffs = effectsOf completedIng alchemyData
          positiveEffs = Set.filter otherIngContains completedEffs
          negativeEffs = Set.filter otherIngNotContains completedEffs

          otherIngContains eff = (otherIng `hasEffect` eff) alchemyData
          otherIngNotContains eff = (otherIng `doesNotHave` eff) alchemyData

          everyEffKnown =
            Set.union positiveEffs negativeEffs == completedEffs
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
    emptyIngredients %= Set.insert ing
  where
    isIngredientUnknown = gets $ Set.null . effectsOf ing


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
    whenM (gets $ Set.member ing . ingredientsNotContaining eff) $
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
        gets $ PairMap.lookup ing . view fullOverlapNonComplete

      forM_ (Map.assocs overlapsWithIng) $ \(otherIng, overlap) ->
        when (Set.notMember eff overlap) $
          incompleteIngNotHasEffRelation %= BR.insert otherIng eff


    ----------------------------------------------------------------------------
    -- Update completed & overlap maps if the ingredient became completed
    ----------------------------------------------------------------------------
    isNewlyComplete <- do
      newIngEffs <- gets $ Set.insert eff . effectsOf ing
      return $ Set.size newIngEffs == 4
    when isNewlyComplete $ do
      -- Add completed ingredient to completed set
      completedIngredients %= Set.insert ing

      -- Remove completed ingredient from maps that should not have
      -- completed ingredients
      fullOverlapNonComplete %= PairMap.delete ing
      incompleteIngNotHasEffRelation %= BR.deleteLeft ing

    ----------------------------------------------------------------------------
    -- Record positive relation between ing and eff
    ----------------------------------------------------------------------------
    ingHasEffRelation %= BR.insert ing eff
    emptyIngredients %= Set.delete ing


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
  -> Set EffectName
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
      let commonEffects = Set.intersection effs1 effs2
      unless (commonEffects `Set.isSubsetOf` effs) $ do
        throwError $ InconsistentOverlapReason $
          "Overlap does not include effects common to both ingredients: " <>
          T.pack (show $ Set.toList commonEffects)

    -- Ensure overlap excludes impossible effects
    do
      let isImpossible eff = do
            impossibleIngs <- gets $ ingredientsNotContaining eff
            return $
              Set.member ing1 impossibleIngs ||
              Set.member ing2 impossibleIngs
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
            when (Set.notMember eff effs) $
              incompleteIngNotHasEffRelation %= BR.insert incompleteIng eff

    isCompleted1 <- gets (isCompleted ing1)
    isCompleted2 <- gets (isCompleted ing2)

    -- Update the effect negatives for noncompleted ingredients
    unless isCompleted1 $ updateNegatives ing1 ing2
    unless isCompleted2 $ updateNegatives ing2 ing1

    -- Update overlap map if both ingredients are not completed
    when (not isCompleted1 && not isCompleted2) $
      fullOverlapNonComplete %= PairMap.insertPair (pair ing1 ing2) effs


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

