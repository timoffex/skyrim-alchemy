{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module AlchemyComponent.IncompleteIngredientNotHasEffectComponent
  ( IncompleteIngredientNotHasEffectComponent
  , knownNonEffectsOfIncompleteIngredient
  , incompleteIngredientsWithoutEffect
  , incompleteDoesNotHave
  ) where


import           AlchemyComponent.CompletedIngredientsComponent
    ( CompletedIngredientsComponent, isCompletedAfterUpdate )
import           AlchemyComponent.Component
    ( AlchemyHas
    , AlchemyHasBefore
    , Component
    , OverlapValidationError (OverlapValidationError)
    )
import qualified AlchemyComponent.Component                             as Component
import           AlchemyComponent.IncompleteIngredientOverlapsComponent
    ( IncompleteIngredientOverlapsComponent
    , knownOverlapsOfIncompleteIngredientsWith
    )
import           AlchemyTypes
    ( EffectName, IngredientName, Overlap (Overlap) )
import           BinaryRelation
    ( BinaryRelation )
import qualified BinaryRelation                                         as BR
import           Data.Function
    ( (&) )
import qualified Data.Map.Strict                                        as Map
import           Data.Set
    ( Set )
import qualified Data.Set                                               as Set
import qualified Data.Text                                              as T


-- | Component that keeps track of the effects that an incomplete ingredient
-- is known to not have.
--
-- This does not track this information for completed ingredients because there
-- these ingredients are known to not have all but a few effects.
data IncompleteIngredientNotHasEffectComponent
  = IncompleteIngredientNotHasEffectComponent
    { _ingNotHasEffRelation :: BinaryRelation IngredientName EffectName }


-- | Returns the set of effects that an incomplete ingredient is known not
-- to have.
--
-- Caution: if the ingredient is completed (i.e. all effects are known), this
-- returns an empty list. This can also return an empty list if no non-effects
-- are known for the ingredient.
knownNonEffectsOfIncompleteIngredient
  :: Component.Has IncompleteIngredientNotHasEffectComponent alchemy
  => IngredientName
  -> alchemy
  -> Set EffectName
knownNonEffectsOfIncompleteIngredient ing
  = BR.byLeft ing . _ingNotHasEffRelation . Component.get


-- | Returns the set of incomplete ingredients known to not have an effect.
incompleteIngredientsWithoutEffect
  :: Component.Has IncompleteIngredientNotHasEffectComponent alchemy
  => EffectName
  -> alchemy
  -> Set IngredientName
incompleteIngredientsWithoutEffect eff
  = BR.byRight eff . _ingNotHasEffRelation . Component.get

-- | Returns whether an incomplete ingredient is known to not have an effect.
--
-- For completed ingredients, this always returns False.
incompleteDoesNotHave
  :: Component.Has IncompleteIngredientNotHasEffectComponent alchemy
  => IngredientName
  -> EffectName
  -> alchemy
  -> Bool
ing `incompleteDoesNotHave` eff
  = BR.check ing eff . _ingNotHasEffRelation . Component.get



instance ( Monad m
         , AlchemyHas IncompleteIngredientOverlapsComponent alchemy
         , AlchemyHasBefore CompletedIngredientsComponent alchemy )
    => Component alchemy m IncompleteIngredientNotHasEffectComponent
  where

    initializeComponent _ = return $
      IncompleteIngredientNotHasEffectComponent BR.empty

    validateOverlap overlap alchemy component
      = return $ validate overlap alchemy component

    componentLearnOverlap overlap alchemy component
      = return $ learn overlap alchemy component


validate (Overlap ing1 ing2 effects) _ component
  | ing1CannotHaveEffect = Just $ ing1CannotHaveEffectError
  | ing2CannotHaveEffect = Just $ ing2CannotHaveEffectError
  | otherwise                = Nothing
  where
    nonEffects ing = BR.byLeft ing $ _ingNotHasEffRelation component

    ing1BadEffects = effects `Set.intersection` nonEffects ing1
    ing2BadEffects = effects `Set.intersection` nonEffects ing2

    ing1CannotHaveEffect = not (Set.null ing1BadEffects)
    ing2CannotHaveEffect = not (Set.null ing2BadEffects)

    ing1CannotHaveEffectError = makeError ing1 ing1BadEffects
    ing2CannotHaveEffectError = makeError ing2 ing2BadEffects

    -- TODO: It would be slightly nicer to print the contradicting overlap
    makeError ing badEffects = OverlapValidationError $
      "Ingredient " <> T.pack (show ing) <> " cannot have effects " <>
      T.pack (show $ Set.toList badEffects) <> " because a contradicting" <>
      " overlap is known."


learn (Overlap ing1 ing2 effects) alchemy component
    = IncompleteIngredientNotHasEffectComponent $
        _ingNotHasEffRelation component
          & updateOverlappingIngredients ing1
          & updateOverlappingIngredients ing2
          & removeIfNewlyComplete ing1
          & removeIfNewlyComplete ing2
  where

    -- For all other incomplete ingredients overlapping with @ing@, the newly
    -- learned effects of @ing@ are non-effects, since otherwise they would
    -- have been in the original overlap and therefore would have already been
    -- known.
    updateOverlappingIngredients ing ingNotHasEffRelation
      = Map.foldlWithKey'
          updateNonEffects
          ingNotHasEffRelation
          (knownOverlapsOfIncompleteIngredientsWith ing alchemy)
    updateNonEffects ingNotHasEffRelation incompleteIng knownOverlap
      = Set.foldl'
          (\br nonEffect -> BR.insert incompleteIng nonEffect br)
          ingNotHasEffRelation
          (effects `Set.difference` knownOverlap)

    removeIfNewlyComplete ing
      = if isCompletedAfterUpdate ing alchemy
        then BR.deleteLeft ing
        else id
