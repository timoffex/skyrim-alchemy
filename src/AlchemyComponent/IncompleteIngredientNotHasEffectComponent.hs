{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module AlchemyComponent.IncompleteIngredientNotHasEffectComponent
  ( IncompleteIngredientNotHasEffectComponent,
    knownNonEffectsOfIncompleteIngredient,
    incompleteIngredientsWithoutEffect,
    incompleteDoesNotHave,
  )
where

import AlchemyComponent.CompletedIngredientsComponent
  ( CompletedIngredientsComponent,
    isCompletedAfterUpdate,
  )
import AlchemyComponent.Component
  ( AlchemyHas,
    AlchemyHasBefore,
    Component,
    ValidationError (ValidationError),
  )
import qualified AlchemyComponent.Component as Component
import AlchemyComponent.IncompleteIngredientOverlapsComponent
  ( IncompleteIngredientOverlapsComponent,
    knownOverlapsOfIncompleteIngredientsWith,
  )
import AlchemyTypes
  ( EffectName,
    IngredientName,
    Overlap (Overlap),
  )
import BinaryRelation
  ( BinaryRelation,
  )
import qualified BinaryRelation as BR
import qualified Control.Algebra as Algebra
import Control.Carrier.State.Strict
  ( execState,
  )
import Control.Effect.Error
  ( throwError,
  )
import qualified Control.Effect.State as State
import Control.Monad
  ( forM_,
  )
import Data.Function
  ( (&),
  )
import qualified Data.Map.Strict as Map
import Data.Set
  ( Set,
  )
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Component that keeps track of the effects that an incomplete ingredient
-- is known to not have.
--
-- This does not track this information for completed ingredients because there
-- these ingredients are known to not have all but a few effects.
data IncompleteIngredientNotHasEffectComponent = IncompleteIngredientNotHasEffectComponent
  {_ingNotHasEffRelation :: BinaryRelation IngredientName EffectName}

-- | Returns the set of effects that an incomplete ingredient is known not
-- to have.
--
-- Caution: if the ingredient is completed (i.e. all effects are known), this
-- returns an empty list. This can also return an empty list if no non-effects
-- are known for the ingredient.
knownNonEffectsOfIncompleteIngredient ::
  Component.Has IncompleteIngredientNotHasEffectComponent alchemy =>
  IngredientName ->
  alchemy ->
  Set EffectName
knownNonEffectsOfIncompleteIngredient ing =
  BR.byLeft ing . _ingNotHasEffRelation . Component.get

-- | Returns the set of incomplete ingredients known to not have an effect.
incompleteIngredientsWithoutEffect ::
  Component.Has IncompleteIngredientNotHasEffectComponent alchemy =>
  EffectName ->
  alchemy ->
  Set IngredientName
incompleteIngredientsWithoutEffect eff =
  BR.byRight eff . _ingNotHasEffRelation . Component.get

-- | Returns whether an incomplete ingredient is known to not have an effect.
--
-- For completed ingredients, this always returns False.
incompleteDoesNotHave ::
  Component.Has IncompleteIngredientNotHasEffectComponent alchemy =>
  IngredientName ->
  EffectName ->
  alchemy ->
  Bool
ing `incompleteDoesNotHave` eff =
  BR.check ing eff . _ingNotHasEffRelation . Component.get

instance
  ( Algebra.Algebra sig m,
    AlchemyHas IncompleteIngredientOverlapsComponent alchemy,
    AlchemyHasBefore CompletedIngredientsComponent alchemy
  ) =>
  Component alchemy m IncompleteIngredientNotHasEffectComponent
  where
  initializeComponent _ =
    return $
      IncompleteIngredientNotHasEffectComponent BR.empty

  componentLearnEffect ing eff alchemy component =
    do
      validateNewEffect ing eff component
      learnEffect ing eff alchemy component

  componentLearnOverlap overlap alchemy component =
    learn overlap alchemy component

validateNewEffect ing eff component
  | ingNotHasEffect = throwError ingNotHasEffectError
  | otherwise = return ()
  where
    ingNotHasEffect = BR.check ing eff $ _ingNotHasEffRelation component
    ingNotHasEffectError =
      ValidationError $
        "Ingredient " <> T.pack (show ing) <> " cannot have effect "
          <> T.pack (show eff)
          <> " because a contradicting overlap is known."

learnEffect ing eff alchemy component =
  do
    validateNewEffect ing eff component
    return $
      IncompleteIngredientNotHasEffectComponent $
        _ingNotHasEffRelation component
          & updateOverlappingIngredients
          & removeIfNewlyComplete
  where
    -- For all other incomplete ingredients overlapping with @ing@, the newly
    -- learned effects of @ing@ are non-effects, since otherwise they would
    -- have been in the original overlap and therefore would have already been
    -- known.
    updateOverlappingIngredients ingNotHasEffRelation =
      Map.foldlWithKey'
        updateNonEffects
        ingNotHasEffRelation
        (knownOverlapsOfIncompleteIngredientsWith ing alchemy)
    updateNonEffects ingNotHasEffRelation incompleteIng knownOverlap =
      if Set.member eff knownOverlap
        then ingNotHasEffRelation
        else BR.insert incompleteIng eff ingNotHasEffRelation

    removeIfNewlyComplete =
      if isCompletedAfterUpdate ing alchemy
        then BR.deleteLeft ing
        else id

learn (Overlap ing1 ing2 effects) alchemy component =
  execState component $ do
    forM_ effects $ \effect -> do
      modifyM $ learnEffect ing1 effect alchemy
      modifyM $ learnEffect ing2 effect alchemy

-- TODO: Move this to a common place or find way to avoid using it
modifyM ::
  Algebra.Has (State.State s) sig m =>
  (s -> m s) ->
  m ()
modifyM action = do
  state <- State.get
  updated <- action state
  State.put updated
