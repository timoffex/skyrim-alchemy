{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module CLISpec
  ( spec,
  )
where

import AlchemyData (AlchemyData, EffectName, IngredientName, ValidationError, effectName, emptyAlchemyData, ingredientName, learnIngredientEffect)
import AlchemyInteraction (AlchemyInteraction (PrintEffects), geq)
import CLI (Command (runCommand), parseCommand)
import Control.Algebra (Handler, run)
import Control.Carrier.Error.Either (ErrorC)
import Control.Carrier.Error.Extra (catching)
import Control.Carrier.Interpret (InterpretC, Interpreter, Reifies, runInterpretState)
import Control.Carrier.Lift (LiftC, runM)
import Control.Carrier.State.Strict (StateC, execState)
import Control.Monad (unless)
import Data.Coerce (coerce)
import Data.Functor (void, ($>))
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Type.Equality ((:~:) (Refl))
import Test.Hspec (SpecWith, specify)

wheat :: IngredientName
wheat = ingredientName "Wheat"

blueButterflyWing :: IngredientName
blueButterflyWing = ingredientName "Blue Butterfly Wing"

fortifyHealth :: EffectName
fortifyHealth = effectName "Fortify Health"

fortifyConjuration :: EffectName
fortifyConjuration = effectName "Fortify Conjuration"

spec :: SpecWith ()
spec = do
  specify "effects of ingredient" $
    runAlchemyData $ do
      learnIngredientEffect wheat fortifyHealth
      expectingInteractions [PrintEffects $ S.singleton fortifyHealth] $
        parseCommand "effects of wheat" >>= runCommand . fromJust

  specify "potential effects of ingredient" $
    runAlchemyData $ do
      learnIngredientEffect wheat fortifyHealth
      learnIngredientEffect blueButterflyWing fortifyConjuration
      expectingInteractions [PrintEffects $ S.singleton fortifyHealth] $ do
        Just cmd <- parseCommand "potential effects of blue butterfly wing"
        runCommand cmd

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

runAlchemyData ::
  StateC
    AlchemyData
    ( ErrorC
        ValidationError
        ( LiftC IO
        )
    )
    a ->
  IO ()
runAlchemyData =
  runM @IO
    . expectingNoErrors @ValidationError
    . void
    . execState (run emptyAlchemyData)

expectingNoErrors ::
  (Show e, Monad m) =>
  ErrorC e m a ->
  m a
expectingNoErrors = flip catching (error . show)

expectingInteractions ::
  ( Monad m,
    eff ~ AlchemyInteraction,
    s ~ [AlchemyInteraction m ()]
  ) =>
  s ->
  ( forall t.
    Reifies t (Interpreter eff (StateC s m)) =>
    InterpretC t eff (StateC s m) a
  ) ->
  m a
expectingInteractions orderedInteractions m = do
  (s, a) <- runInterpretState impl orderedInteractions m
  unless (null s) $
    error $ "Didn't encounter interactions: " ++ show s
  return a
  where
    impl ::
      (Monad m, Functor ctx) =>
      Handler ctx n (StateC s m) ->
      AlchemyInteraction n x ->
      [AlchemyInteraction m ()] ->
      ctx () ->
      m ([AlchemyInteraction m ()], ctx x)
    impl _ eff [] _ = error $ "Unexpected interaction: " ++ show eff
    impl _ eff (i : is) ctx =
      let i' = coerce i
       in case eff `geq` i' of
            Nothing -> error $ "Unexpected interaction: " ++ show eff
            Just Refl ->
              if eff /= i'
                then error $ "Unexpected interaction: " ++ show eff
                else return (is, ctx $> ())
