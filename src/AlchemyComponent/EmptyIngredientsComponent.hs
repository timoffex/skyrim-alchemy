{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module AlchemyComponent.EmptyIngredientsComponent
  ( EmptyIngredientsComponent,
  )
where

import AlchemyComponent.CompletedIngredientsComponent (CompletedIngredientsComponent, isCompletedAfterUpdate)
import AlchemyComponent.Component (Component)
import qualified AlchemyComponent.Component as Component
import AlchemyTypes (IngredientName)
import qualified Control.Algebra as Algebra
import Data.Set (Set)
import qualified Data.Set as Set

newtype EmptyIngredientsComponent = EmptyIngredientsComponent
  {emptyIngredients :: Set IngredientName}

instance
  ( Algebra.Algebra sig m,
    Component.AlchemyHasBefore CompletedIngredientsComponent alchemy
  ) =>
  Component alchemy m EmptyIngredientsComponent
  where
  initializeComponent _ = return $ EmptyIngredientsComponent Set.empty
  componentLearnEffect = learnEffect

learnEffect ing _ alchemy component =
  return $
    if isCompletedAfterUpdate ing alchemy
      then EmptyIngredientsComponent $ Set.delete ing $ emptyIngredients component
      else component
