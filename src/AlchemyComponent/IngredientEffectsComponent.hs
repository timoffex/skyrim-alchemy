{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AlchemyComponent.IngredientEffectsComponent
  ( IngredientEffectsComponent
  , HasIngredientEffectsComponent

  , effectsOf
  , allKnownEffects
  , allIngredientsWithEffects

  , isCompleted
  , allCompletedIngredients
  ) where


import qualified AlchemyComponent.Component as Component
import           AlchemyData
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

    , _completedIngredients :: !(Set IngredientName)
      -- ^ Set of ingredients with all 4 effects known.
      --
      -- We keep track of this here instead of in a separate component so that
      -- we can update the set in a natural way: by checking if updated
      -- ingredients now have 4 effects. This can't be done in a separate
      -- component because components are updated in parallel, so another
      -- component can't tell that some ingredients are completed until this
      -- component is updated (unless it duplicates a bunch of logic).
    }

type HasIngredientEffectsComponent alchemy =
  Component.Has IngredientEffectsComponent alchemy


-- | Gets the known effects of the ingredient.
effectsOf
  :: HasIngredientEffectsComponent alchemy
  => IngredientName -> alchemy -> Set EffectName
effectsOf ing = BR.byLeft ing . _ingHasEffectRelation . Component.get

-- | Gets the set of all effects that are at least on one ingredient.
allKnownEffects
  :: HasIngredientEffectsComponent alchemy
  => alchemy -> Set EffectName
allKnownEffects = BR.rights . _ingHasEffectRelation . Component.get

-- | Gets the set of all ingredients for which at least one effect is known.
allIngredientsWithEffects
  :: HasIngredientEffectsComponent alchemy
  => alchemy -> Set IngredientName
allIngredientsWithEffects = BR.lefts . _ingHasEffectRelation . Component.get

-- | The set of ingredients for which all effects are known.
isCompleted
  :: HasIngredientEffectsComponent alchemy
  => IngredientName
  -> alchemy
  -> Bool
isCompleted ing = Set.member ing . allCompletedIngredients

-- | The set of ingredients with 4 known effects.
allCompletedIngredients
  :: HasIngredientEffectsComponent alchemy
  => alchemy
  -> Set IngredientName
allCompletedIngredients = _completedIngredients . Component.get


instance
    ( Monad m
    ) => Component.Component alchemy m IngredientEffectsComponent where
  initializeComponent _ = return $ IngredientEffectsComponent BR.empty Set.empty
  componentLearnOverlap overlap _ ingredientEffects =
    return $ insertOverlap overlap ingredientEffects


insertOverlap
    (Overlap ing1 ing2 effects)
    component =
  IngredientEffectsComponent ingHasEffectRelation completedIngredients
    where
      ingHasEffectRelation =
        Set.foldl' addEffect (_ingHasEffectRelation component) effects
      addEffect br effect =
        br & BR.insert ing1 effect
           . BR.insert ing2 effect
      
      -- TODO: Add a post-validate that checks that the number of effects
      -- does not exceed 4 for any ingredient
      completedIngredients =
        _completedIngredients component
          & (if checkNewlyCompleted ing1 then Set.insert ing1 else id)
          . (if checkNewlyCompleted ing2 then Set.insert ing2 else id)
      checkNewlyCompleted ing =
        Set.size (BR.byLeft ing ingHasEffectRelation) >= 4
        
