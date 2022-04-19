{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module AlchemyComponent.IncompleteIngredientOverlapsComponent
  ( IncompleteIngredientOverlapsComponent,
    overlapBetweenIncompleteIngredients,
    knownOverlapsOfIncompleteIngredientsWith,
    knownOverlapsOfIncompleteIngredients,
  )
where

import AlchemyComponent.Component as Component
  ( AlchemyHas,
    ValidationError (ValidationError),
  )
import qualified AlchemyComponent.Component as Component
import AlchemyComponent.IngredientEffectsComponent
  ( IngredientEffectsComponent,
    effectsOf,
  )
import AlchemyTypes
  ( EffectName,
    IngredientName,
    Overlap (Overlap),
  )
import qualified Control.Algebra as Algebra
import Control.Effect.Error
  ( throwError,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Maybe
  ( isJust,
  )
import Data.Set
  ( Set,
  )
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.UPair
  ( UPair,
    pair,
  )
import PairMap
  ( PairMap,
  )
import qualified PairMap

-- | Component that keeps track of the known overlaps between incomplete
-- ingredients.
--
-- Requires 'IngredientEffectsComponent'.
newtype IncompleteIngredientOverlapsComponent = IncompleteIngredientOverlapsComponent
  {getPairMap :: PairMap IngredientName (Set EffectName)}

-- | Gets the overlap between two incomplete ingredients if known.
overlapBetweenIncompleteIngredients ::
  Component.Has IncompleteIngredientOverlapsComponent alchemy =>
  IngredientName ->
  IngredientName ->
  alchemy ->
  Maybe (Set EffectName)
overlapBetweenIncompleteIngredients ing1 ing2 =
  PairMap.lookupPair (pair ing1 ing2)
    . getPairMap
    . Component.get

-- | Gets all known overlaps of the given incomplete ingredient with other
-- incomplete ingredients.
knownOverlapsOfIncompleteIngredientsWith ::
  Component.Has IncompleteIngredientOverlapsComponent alchemy =>
  IngredientName ->
  alchemy ->
  Map IngredientName (Set EffectName)
knownOverlapsOfIncompleteIngredientsWith ing =
  PairMap.lookup ing . getPairMap . Component.get

-- | Gets all known overlaps where at least one ingredient is incomplete.
knownOverlapsOfIncompleteIngredients ::
  Component.Has IncompleteIngredientOverlapsComponent alchemy =>
  alchemy ->
  [(UPair IngredientName, (Set EffectName))]
knownOverlapsOfIncompleteIngredients =
  PairMap.assocs . getPairMap . Component.get

instance
  ( Algebra.Algebra sig m,
    AlchemyHas IngredientEffectsComponent alchemy
  ) =>
  Component.Component alchemy m IncompleteIngredientOverlapsComponent
  where
  initializeComponent _ =
    return $
      IncompleteIngredientOverlapsComponent PairMap.empty

  componentLearnOverlap overlap alchemy ingredientOverlaps =
    do
      validate overlap alchemy ingredientOverlaps
      return $ insertOverlap overlap ingredientOverlaps

insertOverlap
  (Overlap ing1 ing2 effects)
  (IncompleteIngredientOverlapsComponent initialData) =
    -- TODO: Remove overlaps where at least one ingredient is completed!
    IncompleteIngredientOverlapsComponent $
      PairMap.insertPair (pair ing1 ing2) effects initialData

validate
  (Overlap ing1 ing2 effects)
  alchemy
  (IncompleteIngredientOverlapsComponent knownOverlaps)
    | overlapExists = throwError overlapExistsError
    | overlapMissingEffects = throwError overlapMissingEffectsError
    -- TODO
    -- overlapHasImpossibleEffects = throwError overlapHasImpossibleEffectsError
    | otherwise = return ()
    where
      existingOverlap = PairMap.lookupPair (pair ing1 ing2) knownOverlaps
      existingOverlapEffects = let Just effects = existingOverlap in effects
      overlapExists = isJust existingOverlap && existingOverlapEffects /= effects
      overlapExistsError =
        ValidationError $
          "A different overlap between "
            <> T.pack (show ing1)
            <> " and "
            <> T.pack (show ing2)
            <> " is already recorded: "
            <> T.pack (show existingOverlapEffects)

      effectsInCommon =
        Set.intersection
          (effectsOf ing1 alchemy)
          (effectsOf ing2 alchemy)
      overlapMissingEffects = not (effectsInCommon `Set.isSubsetOf` effects)
      overlapMissingEffectsError =
        ValidationError $
          "Overlap does not include effects common to both ingredients: "
            <> T.pack (show $ Set.toList effectsInCommon)
