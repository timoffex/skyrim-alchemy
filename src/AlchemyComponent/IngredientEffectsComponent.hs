{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AlchemyComponent.IngredientEffectsComponent
  ( IngredientEffectsComponent

  , effectsOf
  , allKnownEffects
  , allIngredientsWithEffects

  , updatedEffectsOf
  ) where


import qualified AlchemyComponent.Component as Component
import           AlchemyTypes
    ( EffectName, IngredientName, Overlap (Overlap) )
import           BinaryRelation
    ( BinaryRelation )
import qualified BinaryRelation             as BR
import           Data.Function
    ( (&) )
import           Data.Set
    ( Set )
import qualified Data.Set                   as Set


-- | Component that keeps track of the known effects for each ingredient.
data IngredientEffectsComponent
  = IngredientEffectsComponent
    { _ingHasEffectRelation :: !(BinaryRelation IngredientName EffectName)
      -- ^ Relation "ingredient contains effect".
    }


-- | Gets the known effects of the ingredient.
effectsOf
  :: Component.Has IngredientEffectsComponent alchemy
  => IngredientName -> alchemy -> Set EffectName
effectsOf ing = BR.byLeft ing . _ingHasEffectRelation . Component.get

-- | Gets the updated effects of the ingredient.
updatedEffectsOf
  :: Component.HasUpdated IngredientEffectsComponent alchemy
  => IngredientName -> alchemy -> Set EffectName
updatedEffectsOf ing
  = BR.byLeft ing . _ingHasEffectRelation . Component.getUpdated

-- | Gets the set of all effects that are at least on one ingredient.
allKnownEffects
  :: Component.Has IngredientEffectsComponent alchemy
  => alchemy -> Set EffectName
allKnownEffects = BR.rights . _ingHasEffectRelation . Component.get

-- | Gets the set of all ingredients for which at least one effect is known.
allIngredientsWithEffects
  :: Component.Has IngredientEffectsComponent alchemy
  => alchemy -> Set IngredientName
allIngredientsWithEffects = BR.lefts . _ingHasEffectRelation . Component.get


instance
    ( Monad m
    ) => Component.Component alchemy m IngredientEffectsComponent where
  initializeComponent _ = return $ IngredientEffectsComponent BR.empty
  componentLearnOverlap overlap _ ingredientEffects =
    return $ insertOverlap overlap ingredientEffects


insertOverlap
    (Overlap ing1 ing2 effects)
    component =
  IngredientEffectsComponent ingHasEffectRelation
    where
      ingHasEffectRelation =
        Set.foldl' addEffect (_ingHasEffectRelation component) effects
      addEffect br effect =
        br & BR.insert ing1 effect
           . BR.insert ing2 effect
