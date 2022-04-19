{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module AlchemyComponent.CompletedIngredientsComponent
  ( CompletedIngredientsComponent,
    isCompleted,
    allCompletedIngredients,
    isCompletedAfterUpdate,
  )
where

import AlchemyComponent.Component
  ( AlchemyHasBefore,
    Component,
    ValidationError (ValidationError),
  )
import qualified AlchemyComponent.Component as Component
import AlchemyComponent.IngredientEffectsComponent
  ( IngredientEffectsComponent,
    updatedEffectsOf,
  )
import AlchemyTypes
  ( IngredientName,
    Overlap (Overlap),
  )
import qualified Control.Algebra as Algebra
import Control.Effect.Error
  ( throwError,
  )
import Control.Monad
  ( when,
    (>=>),
  )
import Data.Set
  ( Set,
  )
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Component that keeps track of ingredients with all 4 effects known.
--
-- This requires 'IngredientEffectsComponent' to appear earlier in the
-- component list.
newtype CompletedIngredientsComponent = CompletedIngredientsComponent
  {_completedIngredients :: Set IngredientName}

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

instance
  ( Algebra.Algebra sig m,
    AlchemyHasBefore IngredientEffectsComponent alchemy
  ) =>
  Component alchemy m CompletedIngredientsComponent
  where
  initializeComponent _ = return $ CompletedIngredientsComponent Set.empty

  -- TODO: Validate that ingredients are not already completed

  componentLearnEffect ing _ alchemy component =
    updateIngIfCompleted ing alchemy component

  componentLearnOverlap overlap alchemy component =
    learn overlap alchemy component

updateIngIfCompleted ing alchemy component = do
  when (Set.size (updatedEffectsOf ing alchemy) > 4) $
    throwError $
      ValidationError $
        "Too many effects on ingredient " <> T.pack (show ing)

  return $
    CompletedIngredientsComponent $
      ( if Set.size (updatedEffectsOf ing alchemy) == 4
          then Set.insert ing
          else id
      )
        $ _completedIngredients component

learn (Overlap ing1 ing2 _) alchemy =
  updateIngIfCompleted ing1 alchemy
    >=> updateIngIfCompleted ing2 alchemy
