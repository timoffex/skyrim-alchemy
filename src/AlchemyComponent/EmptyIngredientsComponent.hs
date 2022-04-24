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

import AlchemyComponent.CompletedIngredientsComponent (CompletedIngredientsComponent, isCompletedAfterUpdate)
import AlchemyComponent.Component (Component)
import qualified AlchemyComponent.Component as Component
import AlchemyTypes (IngredientName)
import qualified Control.Algebra as Algebra
import Control.Carrier.State.Strict (execState)
import Control.Effect.Lens ((%=))
import Control.Lens (makeLenses, view)
import Control.Monad (when)
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
    Component.AlchemyHasBefore CompletedIngredientsComponent alchemy
  ) =>
  Component alchemy m EmptyIngredientsComponent
  where
  initializeComponent _ = return $ EmptyIngredientsComponent Set.empty
  componentLearnEffect = learnEffect

  componentLearnIngredient ing _ =
    return
      . EmptyIngredientsComponent
      . Set.insert ing
      . view emptyIngredientsSet

learnEffect ing _ alchemy component = execState component $ do
  when (isCompletedAfterUpdate ing alchemy) $
    emptyIngredientsSet %= Set.delete ing
