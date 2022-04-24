{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | Defines a monadic effect (in the sense of fused-effects) for
-- alchemy-related commandline interactions with a user.
--
-- The purpose of this is to replace IO in some code to make it more
-- testable. Instead of raw Strings, @putStrLn@ and @readline@, the
-- code uses this effect and allows the caller to decide how to
-- interpret it.
module AlchemyInteraction
  ( AlchemyInteraction (..),
    geq,
    exit,
    printError,
    printEffects,
    printIngredients,
    IngredientForEffects (IngredientForEffects),
    printOrderedIngredientsForEffects,
  )
where

import AlchemyData (EffectName, IngredientName)
import Control.Algebra (Has, send)
import Data.Kind (Type)
import qualified Data.Set.Internal as S
import Data.Type.Equality (type (:~:) (..))

-- | An ingredient that was selected for some of its effects.
data IngredientForEffects
  = IngredientForEffects IngredientName (S.Set EffectName)
  deriving (Show, Eq)

data AlchemyInteraction (m :: Type -> Type) a where
  Exit :: AlchemyInteraction m ()
  PrintError ::
    String ->
    AlchemyInteraction m ()
  PrintEffects ::
    S.Set EffectName ->
    AlchemyInteraction m ()
  PrintIngredients ::
    S.Set IngredientName ->
    AlchemyInteraction m ()
  PrintOrderedIngredientsForEffects ::
    [IngredientForEffects] ->
    AlchemyInteraction m ()

deriving instance Eq (AlchemyInteraction m a)

deriving instance Show (AlchemyInteraction m a)

-- | A generalized equality for 'AlchemyInteraction' that outputs a
-- proof of the type arguments being equal.
--
-- This and the 'Eq' instance are helpful for testing.
geq ::
  AlchemyInteraction m a ->
  AlchemyInteraction m b ->
  Maybe (a :~: b)
geq Exit Exit = Just Refl
geq (PrintError _) (PrintError _) = Just Refl
geq (PrintEffects _) (PrintEffects _) = Just Refl
geq (PrintIngredients _) (PrintIngredients _) = Just Refl
geq
  (PrintOrderedIngredientsForEffects _)
  (PrintOrderedIngredientsForEffects _) = Just Refl
geq _ _ = Nothing

exit :: Has AlchemyInteraction sig m => m ()
exit = send Exit

printError ::
  Has AlchemyInteraction sig m =>
  String ->
  m ()
printError = send . PrintError

printEffects ::
  Has AlchemyInteraction sig m =>
  S.Set EffectName ->
  m ()
printEffects = send . PrintEffects

printIngredients ::
  Has AlchemyInteraction sig m =>
  S.Set IngredientName ->
  m ()
printIngredients = send . PrintIngredients

printOrderedIngredientsForEffects ::
  Has AlchemyInteraction sig m =>
  [IngredientForEffects] ->
  m ()
printOrderedIngredientsForEffects =
  send . PrintOrderedIngredientsForEffects
