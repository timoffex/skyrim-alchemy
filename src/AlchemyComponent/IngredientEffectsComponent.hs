{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AlchemyComponent.IngredientEffectsComponent
  ( IngredientEffectsComponent
  , HasIngredientEffectsComponent

  , effectsOf
  , allKnownEffects
  , allIngredientsWithEffects
  ) where


import qualified AlchemyComponent.Component as Component
import           AlchemyData
    ( Overlap (Overlap), IngredientName, EffectName )
import           BinaryRelation
    ( BinaryRelation )
import qualified BinaryRelation             as BR
import           Data.Function
    ( (&) )
import           Data.Set
    ( Set )
import qualified Data.Set                   as Set


-- | Component that keeps track of the known effects for each ingredient.
newtype IngredientEffectsComponent = IngredientEffectsComponent
  (BinaryRelation IngredientName EffectName)


type HasIngredientEffectsComponent alchemy =
  Component.Has IngredientEffectsComponent alchemy


getComponent alchemy =
  let IngredientEffectsComponent br = Component.get alchemy
  in br


-- | Gets the known effects of the ingredient.
effectsOf
  :: HasIngredientEffectsComponent alchemy
  => IngredientName -> alchemy -> Set EffectName
effectsOf ing = BR.byLeft ing . getComponent 

-- | Gets the set of all effects that are at least on one ingredient.
allKnownEffects
  :: HasIngredientEffectsComponent alchemy
  => alchemy -> Set EffectName
allKnownEffects = BR.rights . getComponent

-- | Gets the set of all ingredients for which at least one effect is known.
allIngredientsWithEffects
  :: HasIngredientEffectsComponent alchemy
  => alchemy -> Set IngredientName
allIngredientsWithEffects = BR.lefts . getComponent


instance
    ( Monad m
    ) => Component.Component algebra m IngredientEffectsComponent where
  initializeComponent _ = return $ IngredientEffectsComponent BR.empty
  componentLearnOverlap overlap _ ingredientEffects =
    return $ insertOverlap overlap ingredientEffects


insertOverlap
    (Overlap ing1 ing2 effects)
    (IngredientEffectsComponent initialData) =
  IngredientEffectsComponent $ Set.foldl' addEffect initialData effects
    where
      -- Adds the effect to both ing1 and ing2
      addEffect br effect =
        br & BR.insert ing1 effect
           . BR.insert ing2 effect