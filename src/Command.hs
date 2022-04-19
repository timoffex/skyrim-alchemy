{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

-- | Defines user commands for updating and querying alchemy data.
module Command
  ( Command
  , runCommand

  , exit
  , listEffects
  , listEffectsOf
  , listNonEffectsOf
  , listPotentialEffectsOf
  , listIngredients
  , listIngredientsWithAllOf
  , suggestCombineWith
  , learnOverlap
  , learnIngredientEffect
  ) where


import qualified AlchemyData                  as AD
import qualified AlchemyInteraction           as Interaction
import           Control.Algebra
    ( Has )
import           Control.Carrier.Error.Extra
    ( catching, rethrowing )
import           Control.Carrier.State.Strict
    ( State, execState, get, gets, modify )
import           Control.Effect.Error
    ( Error )
import           Control.Effect.State ()
import           Control.Monad
    ( forM, unless )
import           Data.Foldable
    ( maximumBy )
import           Data.Function
    ( fix, on )
import qualified Data.Set                     as S
import qualified Data.Text                    as T


-- | A command for interacting with an 'AD.AlchemyData'.
newtype Command
  = Command
    { runCommand :: forall m sig. HasCommandEffects sig m => m () }


type HasCommandEffects sig m =
  ( Has Interaction.AlchemyInteraction sig m
  , Has (State AD.AlchemyData) sig m )


-- | Exits the program.
exit :: Command
exit = Command Interaction.exit


--------------------------------------------------------------------------------
-- Queries returning effects
--------------------------------------------------------------------------------

listEffects :: Command
listEffects = Command $
  pleaseListEffects >>= Interaction.printEffects

listEffectsOf :: AD.IngredientName -> Command
listEffectsOf ing = Command $
  pleaseListEffectsOf ing >>= Interaction.printEffects

listNonEffectsOf :: AD.IngredientName -> Command
listNonEffectsOf ing = Command $
  pleaseListNonEffectsOf ing >>= Interaction.printEffects

listPotentialEffectsOf :: AD.IngredientName -> Command
listPotentialEffectsOf ing = Command $
  pleaseListPotentialEffectsOf ing >>= Interaction.printEffects


--------------------------------------------------------------------------------
-- Queries returning ingredients
--------------------------------------------------------------------------------

listIngredients :: Command
listIngredients = Command $
  pleaseListIngredients >>= Interaction.printIngredients

listIngredientsWithAllOf :: S.Set AD.EffectName -> Command
listIngredientsWithAllOf effs = Command $
  pleaseListIngredientsWithAllOf effs >>= Interaction.printIngredients

suggestCombineWith :: AD.IngredientName -> Command
suggestCombineWith ing = Command $
  minimumPotentialEffectsCoverOf ing >>=
  Interaction.printOrderedIngredientsForEffects


--------------------------------------------------------------------------------
-- Mutations
--------------------------------------------------------------------------------

learnOverlap
  :: AD.IngredientName
  -> AD.IngredientName
  -> S.Set AD.EffectName
  -> Command
learnOverlap ing1 ing2 effs = Command $
  catching (tryLearnOverlap ing1 ing2 effs) $ \err ->
    Interaction.printError $ T.unpack err

learnIngredientEffect
  :: AD.IngredientName
  -> S.Set AD.EffectName
  -> Command
learnIngredientEffect ing effs = Command $
  catching (mapM_ (tryLearnIngredientEffect ing) effs) $ \err ->
    Interaction.printError $ T.unpack err


--------------------------------------------------------------------------------
-- Command implementation helpers
--------------------------------------------------------------------------------

-- | Approximates a minimum cover of the potential unknown effects on
-- the ingredient.
minimumPotentialEffectsCoverOf
  :: Has (State AD.AlchemyData) sig m
  => AD.IngredientName
  -> m [Interaction.IngredientForEffects]
minimumPotentialEffectsCoverOf ing = do

  -- Approximate a minimum cover by using the greedy algorithm: at
  -- each step, pick the ingredient that contains the largest number
  -- of effects that are still missing.
  potentialEffs <- pleaseListPotentialEffectsOf ing

  ings <- execState ([] @Interaction.IngredientForEffects) $
    fix $ \loop -> do
    ingsSoFar <- get @[Interaction.IngredientForEffects]

    let
      effects (Interaction.IngredientForEffects _ effs) = effs
      effsSoFar = S.unions (effects <$> ingsSoFar)
      missingEffs = potentialEffs `S.difference` effsSoFar

    unless (S.null missingEffs) $ do
      ingsForMissingEffs <- pleaseListIngredientsWithAnyOf missingEffs

      rankedIngsWithEffs <- forM (S.toList ingsForMissingEffs) $
        \ingToRank -> do
        ingEffs <- pleaseListEffectsOf ingToRank
        let importantEffs = S.intersection ingEffs missingEffs
        return
          ( Interaction.IngredientForEffects
              ingToRank
              importantEffs
          , S.size importantEffs
          )

      -- maximumBy requires rankedIngsWithEffs to be non-empty. It is
      -- non-empty because there missingEffs is non-empty, and there
      -- is at least one ingredient for every effect (because that's
      -- the only way for an effect to be known).
      modify $ (:) $
        fst $ maximumBy (compare `on` snd) rankedIngsWithEffs

      loop

  return $ reverse ings

pleaseListEffects
  :: Has (State AD.AlchemyData) sig m
  => m (S.Set AD.EffectName)
pleaseListEffects = gets @AD.AlchemyData AD.allKnownEffects

pleaseListIngredients
  :: Has (State AD.AlchemyData) sig m
  => m (S.Set AD.IngredientName)
pleaseListIngredients = gets AD.allKnownIngredients


pleaseListIngredientsWith
  :: Has (State AD.AlchemyData) sig m
  => AD.EffectName
  -> m (S.Set AD.IngredientName)
pleaseListIngredientsWith = pleaseListIngredientsWithAnyOf . S.singleton

pleaseListIngredientsWithAnyOf
  :: Has (State AD.AlchemyData) sig m
  => S.Set AD.EffectName
  -> m (S.Set AD.IngredientName)
pleaseListIngredientsWithAnyOf = gets . AD.ingredientsWithAnyOf

pleaseListIngredientsWithAllOf
  :: Has (State AD.AlchemyData) sig m
  => S.Set AD.EffectName
  -> m (S.Set AD.IngredientName)
pleaseListIngredientsWithAllOf effs = do
  ingsPerEff <- mapM pleaseListIngredientsWith (S.toList effs)
  if null ingsPerEff
    then return S.empty
    else return $ foldl1 S.intersection ingsPerEff

pleaseListEffectsOf
  :: Has (State AD.AlchemyData) sig m
  => AD.IngredientName
  -> m (S.Set AD.EffectName)
pleaseListEffectsOf = gets @AD.AlchemyData . AD.effectsOf

pleaseListNonEffectsOf
  :: Has (State AD.AlchemyData) sig m
  => AD.IngredientName
  -> m (S.Set AD.EffectName)
pleaseListNonEffectsOf = gets . AD.nonEffectsOf

pleaseListPotentialEffectsOf
  :: Has (State AD.AlchemyData) sig m
  => AD.IngredientName
  -> m (S.Set AD.EffectName)
pleaseListPotentialEffectsOf ing = do
  allEffs <- pleaseListEffects
  nonEffs <- pleaseListNonEffectsOf ing
  ingEffs <- pleaseListEffectsOf ing
  return $ allEffs `S.difference` (nonEffs `S.union` ingEffs)

tryLearnOverlap
  :: ( Has (State AD.AlchemyData) sig m
     , Has (Error T.Text) sig m )
  => AD.IngredientName
  -> AD.IngredientName
  -> S.Set AD.EffectName
  -> m ()
tryLearnOverlap ing1 ing2 effs =
  rethrowing @AD.ValidationError (T.pack . show) $
  AD.learnOverlap ing1 ing2 effs


tryLearnIngredientEffect
  :: ( Has (State AD.AlchemyData) sig m
     , Has (Error T.Text) sig m )
  => AD.IngredientName
  -> AD.EffectName
  -> m ()
tryLearnIngredientEffect ing eff =
  rethrowing @AD.ValidationError (T.pack . show) $
  AD.learnIngredientEffect ing eff
