{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module AlchemyComponent.EmptyIngredientsComponent
  ( EmptyIngredientsComponent,
    emptyIngredients,
  )
where

import AlchemyComponent.Component (Component)
import qualified AlchemyComponent.Component as Component
import AlchemyComponent.IngredientEffectsComponent (IngredientEffectsComponent, updatedEffectsOf)
import AlchemyTypes (IngredientName)
import qualified Control.Algebra as Algebra
import Control.Carrier.State.Strict (execState)
import Control.Effect.Lens ((%=))
import Control.Lens (makeLenses, view)
import Data.Set (Set)
import qualified Data.Set as Set

-- | Component that keeps track of ingredients for which no effects are known.
newtype EmptyIngredientsComponent = EmptyIngredientsComponent
  {_emptyIngredientsSet :: Set IngredientName}

$(makeLenses ''EmptyIngredientsComponent)

-- | The set of known ingredients for which no effects are known.
emptyIngredients ::
  Component.Has EmptyIngredientsComponent alchemy =>
  alchemy ->
  Set IngredientName
emptyIngredients = view emptyIngredientsSet . Component.get

instance
  ( Algebra.Algebra sig m,
    Component.AlchemyHasBefore IngredientEffectsComponent alchemy
  ) =>
  Component alchemy m EmptyIngredientsComponent
  where
  initializeComponent _ = return $ EmptyIngredientsComponent Set.empty

  componentLearnIngredient ing alchemy component = execState component $ do
    if Set.null (updatedEffectsOf ing alchemy)
      then emptyIngredientsSet %= Set.insert ing
      else emptyIngredientsSet %= Set.delete ing