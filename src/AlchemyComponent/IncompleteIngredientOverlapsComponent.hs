{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module AlchemyComponent.IncompleteIngredientOverlapsComponent
  ( IncompleteIngredientOverlapsComponent
  , HasIncompleteIngredientOverlapsComponent

  , overlapBetweenIncompleteIngredients
  ) where


import           AlchemyComponent.Component                  as Component
    ( OverlapValidationError (OverlapValidationError) )
import qualified AlchemyComponent.Component                  as Component
import           AlchemyComponent.IngredientEffectsComponent
    ( HasIngredientEffectsComponent, effectsOf )
import           AlchemyData
    ( EffectName, IngredientName, Overlap (Overlap) )
import           Data.Maybe
    ( isJust )
import           Data.Set
    ( Set )
import qualified Data.Set                                    as Set
import qualified Data.Text                                   as T
import           Data.UPair
    ( pair )
import           PairMap
    ( PairMap )
import qualified PairMap                                     as PairMap


-- | Component that keeps track of the known effects for each ingredient.
--
-- Requires 'IngredientEffectsComponent'.
newtype IncompleteIngredientOverlapsComponent =
    IncompleteIngredientOverlapsComponent
      (PairMap IngredientName (Set EffectName))

type HasIncompleteIngredientOverlapsComponent alchemy =
  Component.Has IncompleteIngredientOverlapsComponent alchemy

getComponent alchemy =
  let IncompleteIngredientOverlapsComponent pm = Component.get alchemy
  in pm


-- | Gets the overlap between two incomplete ingredients if known.
overlapBetweenIncompleteIngredients
  :: HasIncompleteIngredientOverlapsComponent alchemy
  => IngredientName
  -> IngredientName
  -> alchemy
  -> Maybe (Set EffectName)
overlapBetweenIncompleteIngredients ing1 ing2 =
  PairMap.lookupPair (pair ing1 ing2) . getComponent


instance ( Monad m
         , HasIngredientEffectsComponent alchemy ) =>
    Component.Component alchemy m
        IncompleteIngredientOverlapsComponent
  where
    initializeComponent _ = return $
      IncompleteIngredientOverlapsComponent PairMap.empty

    componentLearnOverlap overlap _ ingredientOverlaps =
      return $ insertOverlap overlap ingredientOverlaps

    validateOverlap overlap alchemy ingredientOverlaps =
      return $ validate overlap alchemy ingredientOverlaps


insertOverlap
    (Overlap ing1 ing2 effects)
    (IncompleteIngredientOverlapsComponent initialData) =
  IncompleteIngredientOverlapsComponent $
      PairMap.insertPair (pair ing1 ing2) effects initialData


validate
    (Overlap ing1 ing2 effects)
    alchemy
    (IncompleteIngredientOverlapsComponent knownOverlaps)
  | overlapExists               = Just overlapExistsError
  | overlapMissingEffects       = Just overlapMissingEffectsError
  -- TODO
  -- | overlapHasImpossibleEffects = Just overlapHasImpossibleEffectsError
  | otherwise                   = Nothing
  where

    existingOverlap = PairMap.lookupPair (pair ing1 ing2) knownOverlaps
    existingOverlapEffects = let Just effects = existingOverlap in effects
    overlapExists = isJust existingOverlap
    overlapExistsError = OverlapValidationError $
        "Overlap between " <>
        T.pack (show ing1) <> " and " <> T.pack (show ing2) <>
        " already known: " <> T.pack (show existingOverlapEffects)

    effectsInCommon = Set.intersection (effectsOf ing1 alchemy)
                                       (effectsOf ing2 alchemy)
    overlapMissingEffects = not (effectsInCommon `Set.isSubsetOf` effects)
    overlapMissingEffectsError = OverlapValidationError $
        "Overlap does not include effects common to both ingredients: " <>
        T.pack (show $ Set.toList effectsInCommon)

