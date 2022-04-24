{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module AlchemyComponent.IngredientEffectsComponent
  ( IngredientEffectsComponent,
    hasEffect,
    effectsOf,
    ingredientsWith,
    allKnownEffects,
    allIngredientsWithEffects,
    updatedEffectsOf,
  )
where

import qualified AlchemyComponent.Component as Component
import AlchemyTypes (EffectName, IngredientName)
import BinaryRelation (BinaryRelation)
import qualified BinaryRelation as BR
import qualified Control.Algebra as Algebra
import Data.Set (Set)
import qualified Data.Set as Set

-- | Component that keeps track of the known effects for each ingredient.
newtype IngredientEffectsComponent = IngredientEffectsComponent
  { -- | Relation "ingredient contains effect".
    _ingHasEffectRelation :: BinaryRelation IngredientName EffectName
  }

-- | Gets the known effects of the ingredient.
effectsOf ::
  Component.Has IngredientEffectsComponent alchemy =>
  IngredientName ->
  alchemy ->
  Set EffectName
effectsOf ing = BR.byLeft ing . _ingHasEffectRelation . Component.get

-- | Gets the updated effects of the ingredient.
updatedEffectsOf ::
  Component.HasUpdated IngredientEffectsComponent alchemy =>
  IngredientName ->
  alchemy ->
  Set EffectName
updatedEffectsOf ing =
  BR.byLeft ing . _ingHasEffectRelation . Component.getUpdated

-- | Returns whether the ingredient is known to have the effect.
hasEffect ::
  Component.Has IngredientEffectsComponent alchemy =>
  IngredientName ->
  EffectName ->
  alchemy ->
  Bool
ing `hasEffect` eff =
  Set.member eff . effectsOf ing

-- | Gets the set of ingredients known to have an effect.
ingredientsWith ::
  Component.Has IngredientEffectsComponent alchemy =>
  EffectName ->
  alchemy ->
  Set IngredientName
ingredientsWith eff = BR.byRight eff . _ingHasEffectRelation . Component.get

-- | Gets the set of all effects that are at least on one ingredient.
allKnownEffects ::
  Component.Has IngredientEffectsComponent alchemy =>
  alchemy ->
  Set EffectName
allKnownEffects = BR.rights . _ingHasEffectRelation . Component.get

-- | Gets the set of all ingredients for which at least one effect is known.
allIngredientsWithEffects ::
  Component.Has IngredientEffectsComponent alchemy =>
  alchemy ->
  Set IngredientName
allIngredientsWithEffects = BR.lefts . _ingHasEffectRelation . Component.get

instance
  ( Algebra.Algebra sig m
  ) =>
  Component.Component alchemy m IngredientEffectsComponent
  where
  initializeComponent _ = return $ IngredientEffectsComponent BR.empty
  componentLearnEffect ing eff _ ingredientEffects =
    return $ insertEffect ing eff ingredientEffects

insertEffect ing eff =
  IngredientEffectsComponent
    . BR.insert ing eff
    . _ingHasEffectRelation
