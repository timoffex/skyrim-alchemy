{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module AlchemyComponent.CompletedIngredientsComponent
  ( CompletedIngredientsComponent,
    isCompleted,
    allCompletedIngredients,
    isCompletedAfterUpdate,
  )
where

import AlchemyComponent.Component (AlchemyHasBefore, Component, ValidationError (ValidationError))
import qualified AlchemyComponent.Component as Component
import AlchemyComponent.IngredientEffectsComponent (IngredientEffectsComponent, updatedEffectsOf)
import AlchemyTypes (IngredientName, Overlap (Overlap))
import qualified Control.Algebra as Algebra
import Control.Carrier.State.Strict (execState)
import Control.Effect.Error (throwError)
import Control.Effect.Lens ((%=))
import Control.Lens (makeLenses)
import Control.Monad (when, (>=>))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Component that keeps track of ingredients with all 4 effects known.
--
-- This requires 'IngredientEffectsComponent' to appear earlier in the
-- component list.
newtype CompletedIngredientsComponent = CompletedIngredientsComponent
  {_completedIngredients :: Set IngredientName}

$(makeLenses ''CompletedIngredientsComponent)

-- | Whether the ingredient has all 4 effects known.
isCompleted ::
  Component.Has CompletedIngredientsComponent alchemy =>
  IngredientName ->
  alchemy ->
  Bool
isCompleted ing = Set.member ing . _completedIngredients . Component.get

-- | Whether the ingredient became complete after the component updated.
isCompletedAfterUpdate ::
  Component.HasUpdated CompletedIngredientsComponent alchemy =>
  IngredientName ->
  alchemy ->
  Bool
isCompletedAfterUpdate ing =
  Set.member ing . _completedIngredients . Component.getUpdated

-- | The set of all ingredients with all 4 effects known.
allCompletedIngredients ::
  Component.Has CompletedIngredientsComponent alchemy =>
  alchemy ->
  Set IngredientName
allCompletedIngredients = _completedIngredients . Component.get

--------------------------------------------------------------------------------
-- Implementation
--------------------------------------------------------------------------------

instance
  ( Algebra.Algebra sig m,
    AlchemyHasBefore IngredientEffectsComponent alchemy
  ) =>
  Component alchemy m CompletedIngredientsComponent
  where
  initializeComponent _ = return $ CompletedIngredientsComponent Set.empty

  componentLearnEffect ing _ alchemy component =
    updateIngIfCompleted ing alchemy component

  componentLearnOverlap (Overlap ing1 ing2 _) alchemy =
    updateIngIfCompleted ing1 alchemy
      >=> updateIngIfCompleted ing2 alchemy

updateIngIfCompleted ing alchemy component = execState component $ do
  let numEffects = Set.size $ updatedEffectsOf ing alchemy

  when (numEffects > 4) $
    throwError $
      ValidationError $
        "Too many effects on ingredient " <> T.pack (show ing)

  when (numEffects == 4) $
    completedIngredients %= Set.insert ing
