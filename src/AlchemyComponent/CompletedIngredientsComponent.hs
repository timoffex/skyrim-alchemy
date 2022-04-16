{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}


module AlchemyComponent.CompletedIngredientsComponent
  ( CompletedIngredientsComponent

  , isCompleted
  , allCompletedIngredients

  , isCompletedAfterUpdate
  ) where


import           AlchemyComponent.Component
    ( AlchemyHasBefore, Component )
import qualified AlchemyComponent.Component                  as Component
import           AlchemyComponent.IngredientEffectsComponent
    ( IngredientEffectsComponent, updatedEffectsOf )
import           AlchemyData
    ( IngredientName, Overlap (Overlap) )
import           Data.Function
    ( (&) )
import           Data.Set
    ( Set )
import qualified Data.Set                                    as Set


-- | Component that keeps track of ingredients with all 4 effects known.
--
-- This requires 'IngredientEffectsComponent' to appear earlier in the
-- component list.
data CompletedIngredientsComponent
  = CompletedIngredientsComponent
    { _completedIngredients :: !(Set IngredientName) }


-- | Whether the ingredient has all 4 effects known.
isCompleted
  :: Component.Has CompletedIngredientsComponent alchemy
  => IngredientName
  -> alchemy
  -> Bool
isCompleted ing = Set.member ing . _completedIngredients . Component.get

-- | Whether the ingredient became complete after the component updated.
isCompletedAfterUpdate
  :: Component.HasUpdated CompletedIngredientsComponent alchemy
  => IngredientName
  -> alchemy
  -> Bool
isCompletedAfterUpdate ing
  = Set.member ing . _completedIngredients . Component.getUpdated

-- | The set of all ingredients with all 4 effects known.
allCompletedIngredients
  :: Component.Has CompletedIngredientsComponent alchemy
  => alchemy
  -> Set IngredientName
allCompletedIngredients = _completedIngredients . Component.get


instance ( Monad m
         , AlchemyHasBefore IngredientEffectsComponent alchemy )
    => Component alchemy m CompletedIngredientsComponent
  where

    initializeComponent _ = return $ CompletedIngredientsComponent Set.empty

    -- TODO: Validate that ingredients are not already completed

    componentLearnOverlap overlap alchemy component
      = return $ learn overlap alchemy component


learn (Overlap ing1 ing2 _) alchemy component
    = CompletedIngredientsComponent newCompletedIngredients
  where
    isIng1Completed = Set.size (updatedEffectsOf ing1 alchemy) >= 4
    isIng2Completed = Set.size (updatedEffectsOf ing2 alchemy) >= 4

    newCompletedIngredients =
      _completedIngredients component
        & (if isIng1Completed then Set.insert ing1 else id)
        . (if isIng2Completed then Set.insert ing2 else id)
