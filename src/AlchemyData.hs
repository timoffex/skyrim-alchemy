{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module AlchemyData
  ( IngredientName,
    ingredientName,
    EffectName,
    effectName,
    AlchemyData,
    Overlap (Overlap),

    -- * Construction
    emptyAlchemyData,
    learnIngredient,
    learnIngredientEffect,
    learnOverlap,

    -- * Queries
    allKnownIngredients,
    allCompletedIngredients,
    allKnownEffects,
    allKnownOverlaps,
    effectsOf,
    effectsOfIngredientIn,
    nonEffectsOf,
    nonEffectsOfIngredientIn,
    ingredientsNotContaining,
    ingredientsNotOverlappingWith,
    ingredientsOverlappingWith,
    overlapBetween,
    isCompleted,
    ingredientsWith,
    ingredientsWithAnyOf,
    ValidationError,
  )
where

import qualified AlchemyComponent.CompletedIngredientsComponent as Component
import AlchemyComponent.Component
  ( AlchemyComponents,
    ValidationError,
  )
import qualified AlchemyComponent.Component as Component
import qualified AlchemyComponent.EmptyIngredientsComponent as Component
import qualified AlchemyComponent.IncompleteIngredientNotHasEffectComponent as Component
import qualified AlchemyComponent.IncompleteIngredientOverlapsComponent as Component
import qualified AlchemyComponent.IngredientEffectsComponent as Component
import AlchemyTypes
  ( EffectName,
    IngredientName,
    Overlap (Overlap),
    effectName,
    ingredientName,
  )
import Control.Algebra
  ( Algebra,
    Has,
  )
import Control.Carrier.Error.Extra
  ( rethrowing,
  )
import Control.Carrier.State.Strict
  ( get,
    put,
  )
import qualified Control.Carrier.State.Strict as State
import Control.Effect.Error
  ( Error,
  )
import Control.Effect.State
  ( State,
  )
import Data.Foldable
  ( Foldable (fold),
  )
import Data.Function
  ( (&),
  )
import Data.List
  ( foldl1',
  )
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( isJust,
  )
import Data.Set
  ( Set,
  )
import qualified Data.Set as Set
import Data.UPair
  ( UPair,
    distinctPairs,
    pair,
    unpair,
  )

type AlchemyData =
  AlchemyComponents
    '[ Component.IngredientEffectsComponent,
       Component.CompletedIngredientsComponent,
       Component.EmptyIngredientsComponent,
       Component.IncompleteIngredientOverlapsComponent,
       Component.IncompleteIngredientNotHasEffectComponent
     ]

-- | An initial 'AlchemyData' that contains no information about
-- effects or ingredients.
emptyAlchemyData :: Algebra sig m => m AlchemyData
emptyAlchemyData = Component.initialize

-- | Whether the ingredient is completed (i.e. has all 4 effects).
isCompleted :: IngredientName -> AlchemyData -> Bool
isCompleted = Component.isCompleted

-- | Gets the set of all completed ingredients.
allCompletedIngredients :: AlchemyData -> Set IngredientName
allCompletedIngredients = Component.allCompletedIngredients

-- | Gets the set of all incomplete ingredients.
allIncompleteIngredients :: AlchemyData -> Set IngredientName
allIncompleteIngredients alchemyData =
  allKnownIngredients alchemyData
    `Set.difference` allCompletedIngredients alchemyData

-- | Gets the set of all known ingredients.
allKnownIngredients :: AlchemyData -> Set IngredientName
allKnownIngredients alchemyData =
  Set.union
    (Component.allIngredientsWithEffects alchemyData)
    (Component.emptyIngredients alchemyData)

-- | Gets all known overlaps between pairs of ingredients.
allKnownOverlaps ::
  AlchemyData ->
  [(UPair IngredientName, Set EffectName)]
allKnownOverlaps alchemyData =
  Component.knownOverlapsOfIncompleteIngredients alchemyData
    ++
    -- Overlaps between completed and incomplete ingredients
    [ (pair ing1 ing2, overlap)
      | ing1 <- Set.toList (allCompletedIngredients alchemyData),
        ing2 <- Set.toList (allIncompleteIngredients alchemyData),
        let maybeOverlap = overlapBetween ing1 ing2 alchemyData
            Just overlap = maybeOverlap,
        isJust maybeOverlap
    ]
    ++
    -- Overlaps between pairs of completed ingredients
    [ (ings, overlap)
      | ings <- distinctPairs (allCompletedIngredients alchemyData),
        let (ing1, ing2) = unpair ings
            maybeOverlap = overlapBetween ing1 ing2 alchemyData
            Just overlap = maybeOverlap,
        isJust maybeOverlap
    ]

allKnownEffects :: AlchemyData -> Set EffectName
allKnownEffects = Component.allKnownEffects

effectsOf :: IngredientName -> AlchemyData -> Set EffectName
effectsOf = Component.effectsOf

effectsOfIngredientIn :: AlchemyData -> IngredientName -> Set EffectName
effectsOfIngredientIn = flip effectsOf

-- | Returns whether an ingredient is known to not have an effect.
doesNotHave :: IngredientName -> EffectName -> AlchemyData -> Bool
doesNotHave ing eff alchemyData
  | isCompleted ing alchemyData = not (hasEffect ing eff alchemyData)
  | otherwise =
    (ing `Component.incompleteDoesNotHave` eff)
      alchemyData

-- | Returns whether an ingredient is known to have an effect.
hasEffect :: IngredientName -> EffectName -> AlchemyData -> Bool
hasEffect = Component.hasEffect

-- | Gets the known non-effects of the ingredient.
nonEffectsOf :: IngredientName -> AlchemyData -> Set EffectName
nonEffectsOf ing alchemyData
  | isIngCompleted = Set.difference allEffs ingEffs
  | otherwise = nonEffectsOfIncomplete
  where
    isIngCompleted = isCompleted ing alchemyData
    allEffs = allKnownEffects alchemyData
    ingEffs = effectsOf ing alchemyData

    nonEffectsOfIncomplete =
      Component.knownNonEffectsOfIncompleteIngredient ing alchemyData

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
      Component.incompleteIngredientsWithoutEffect eff alchemyData
    completeIngsNotContainingEff =
      Set.filter
        (\ing -> not (hasEffect ing eff alchemyData))
        (allCompletedIngredients alchemyData)

-- | Gets the set of ingredients known to contain an effect.
ingredientsWith :: EffectName -> AlchemyData -> Set IngredientName
ingredientsWith = Component.ingredientsWith

ingredientsWithEffectIn :: AlchemyData -> EffectName -> Set IngredientName
ingredientsWithEffectIn = flip ingredientsWith

-- | Gets the set of ingredients known to contain any of the effects.
ingredientsWithAnyOf :: Set EffectName -> AlchemyData -> Set IngredientName
ingredientsWithAnyOf effs alchemyData =
  Set.unions
    ( fmap
        (\eff -> ingredientsWith eff alchemyData)
        (Set.toList effs)
    )

-- | Gets all ingredients known to not overlap with the given ingredient.
ingredientsNotOverlappingWith ::
  IngredientName ->
  AlchemyData ->
  Set IngredientName
ingredientsNotOverlappingWith ing alchemyData
  | isIngCompleted = ingsNotContainingIngEffs
  | otherwise = ingsNotOverlappingIncompleteIng
  where
    isIngCompleted = isCompleted ing alchemyData

    ingsNotContainingIngEffs =
      foldl1'
        Set.intersection
        [ ingredientsNotContaining eff alchemyData
          | eff <- Set.toList $ effectsOf ing alchemyData
        ]

    ingsNotOverlappingIncompleteIng =
      Set.union
        completedIngsNotOverlappingIng
        incompleteIngsNotOverlappingIng

    completedIngsNotOverlappingIng =
      Set.filter
        (\other -> Set.null $ fold $ overlapBetween ing other alchemyData)
        (allCompletedIngredients alchemyData)

    incompleteIngsNotOverlappingIng =
      Component.knownOverlapsOfIncompleteIngredientsWith ing alchemyData
        & Map.filter Set.null
        & Map.keys
        & Set.fromList

-- | Gets all ingredients that overlap with the given ingredient.
ingredientsOverlappingWith ::
  IngredientName ->
  AlchemyData ->
  Set IngredientName
ingredientsOverlappingWith ing alchemyData =
  Set.unions
    ( Set.map (ingredientsWithEffectIn alchemyData) $
        effectsOf ing alchemyData
    )

-- | Gets the full overlap between the two ingredients if it is known.
overlapBetween ::
  IngredientName ->
  IngredientName ->
  AlchemyData ->
  Maybe (Set EffectName)
overlapBetween ing1 ing2 alchemyData
  | isCompleted1 = overlapWithCompleted ing1 ing2
  | isCompleted2 = overlapWithCompleted ing2 ing1
  | otherwise =
    Component.overlapBetweenIncompleteIngredients
      ing1
      ing2
      alchemyData
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
learnIngredient ::
  (Has (State AlchemyData) sig m) =>
  IngredientName ->
  m ()
learnIngredient ing =
  State.get @AlchemyData
    >>= Component.learnIngredient ing
    >>= State.put

-- | Associates the effect to the ingredient.
learnIngredientEffect ::
  ( Has (State AlchemyData) sig m,
    Has (Error ValidationError) sig m
  ) =>
  IngredientName ->
  EffectName ->
  m ()
learnIngredientEffect ing eff =
  State.get @AlchemyData
    >>= rethrowing id . Component.learnIngredientEffect ing eff
    >>= State.put

learnOverlap ::
  ( Has (State AlchemyData) sig m,
    Has (Error ValidationError) sig m
  ) =>
  IngredientName ->
  IngredientName ->
  Set EffectName ->
  m ()
learnOverlap ing1 ing2 effs =
  do
    oldAlchemyData <- get @AlchemyData
    newAlchemyData <-
      rethrowing id $
        Component.learnOverlap (Overlap ing1 ing2 effs) oldAlchemyData
    put newAlchemyData
