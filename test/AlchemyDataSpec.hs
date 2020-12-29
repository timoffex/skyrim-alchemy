{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module AlchemyDataSpec
  ( spec
  ) where

import           AlchemyData
    ( AlchemyData
    , EffectName
    , InconsistentEffect
    , InconsistentOverlap
    , IngredientName
    , allCompletedIngredients
    , allKnownIngredients
    , allKnownOverlaps
    , effectName
    , effectsOf
    , emptyAlchemyData
    , ingredientName
    , isCompleted
    , learnIngredient
    , learnIngredientEffect
    , learnOverlap
    , nonEffectsOf
    )
import           Control.Carrier.Error.Either
    ( ErrorC, runError )
import           Control.Carrier.Error.Extra
    ( catching )
import           Control.Carrier.Lift
    ( LiftC, runM, sendIO )
import           Control.Carrier.State.Strict
    ( StateC, execState, gets )
import           Control.Effect.Lift
    ( Has, Lift )
import           Data.Functor
    ( void )
import qualified Data.Set                     as S
import qualified Data.Text                    as T
import           Test.Hspec
    ( HasCallStack, SpecWith, describe, it, shouldBe, shouldSatisfy )



butterflyWing :: IngredientName
butterflyWing = ingredientName "butterfly wing"

blueMountainFlower :: IngredientName
blueMountainFlower = ingredientName "blue mountain flower"

nirnroot :: IngredientName
nirnroot = ingredientName "nirnroot"

wheat :: IngredientName
wheat = ingredientName "wheat"

restoreHealth :: EffectName
restoreHealth = effectName "restore health"

fortifyHealth :: EffectName
fortifyHealth = effectName "fortify health"


spec :: SpecWith ()
spec = do
  it "allKnownIngredients includes empty ingredient" $ runAlchemyData $ do
    learnIngredient wheat
    gets allKnownIngredients `shouldReturn` S.singleton wheat

  it "cannot add more than 4 effects to an ingredient" $ runAlchemyData $ do
    let ing = ingredientName "Test"
    learnIngredientEffect ing $ effectName "eff1"
    learnIngredientEffect ing $ effectName "eff2"
    learnIngredientEffect ing $ effectName "eff3"
    learnIngredientEffect ing $ effectName "eff4"

    expectThrows @InconsistentEffect $
      learnIngredientEffect ing $ effectName "eff5"

  it "ingredient with 4 effects is completed" $ runAlchemyData $ do
    let
      ing = ingredientName "Test"
      effs = effectName . T.pack . show @Integer <$> [1..4]

    mapM_ (learnIngredientEffect ing) effs
    gets allCompletedIngredients `shouldReturn` S.singleton ing


  describe "allKnownOverlaps" $ do
    it "includes overlaps with completed ingredients" $ runAlchemyData $ do
      let
        completedIng = ingredientName "Completed"
        otherIng = ingredientName "Other"
        completedEffs = effectName . T.pack . show @Integer <$> [1..4]

      mapM_ (learnIngredientEffect completedIng) completedEffs
      learnOverlap completedIng otherIng S.empty
      gets allKnownOverlaps `shouldReturn` [((completedIng, otherIng), S.empty)]


    it "includes overlaps with empty ingredients" $ runAlchemyData $ do
      learnOverlap nirnroot wheat S.empty
      gets allKnownOverlaps `shouldReturn` [((nirnroot, wheat), S.empty)]


  describe "learnIngredientEffect" $ do
    it "learns effect on ingredient" $ runAlchemyData $ do
      learnIngredientEffect wheat restoreHealth
      gets (effectsOf wheat) `shouldReturn` S.fromList [restoreHealth]

    it "learns completed ingredient" $ runAlchemyData $ do
      mapM_ (learnIngredientEffect wheat) $
        effectName <$> ["e1", "e2", "e3", "e4"]
      gets (isCompleted wheat) `shouldReturn` True

    it "learns that overlapping ingredients don't have effect" $
      runAlchemyData $ do
      learnOverlap blueMountainFlower butterflyWing $
        S.fromList [restoreHealth]
      learnIngredientEffect blueMountainFlower fortifyHealth
      gets (nonEffectsOf butterflyWing)
        `shouldReturnSatisfying` S.member fortifyHealth


  describe "learnOverlap" $ do
    it "can learn overlap with completed ingredient" $ runAlchemyData $ do
      let
        completedIng = ingredientName "Completed"
        otherIng = ingredientName "Other"

        completedEffs = effectName . T.pack . show @Integer <$> [1..4]

      mapM_ (learnIngredientEffect completedIng) completedEffs
      learnOverlap completedIng otherIng S.empty
      gets allKnownOverlaps `shouldReturn` [((completedIng, otherIng), S.empty)]

    it "can learn overlap that completes an ingredient" $ runAlchemyData $ do
      let
        ing1 = ingredientName "ing1"
        ing2 = ingredientName "ing2"

        overlap = S.fromList $ effectName <$>
          [ "eff2", "eff3", "eff4" ]

      learnIngredientEffect ing1 $ effectName "eff1"
      learnOverlap ing1 ing2 overlap

      gets (isCompleted ing1) `shouldReturn` True
      gets allKnownOverlaps `shouldReturn` [((ing1, ing2), overlap)]

    it "cannot learn overlap that contradicts the existing one" $
      runAlchemyData $ do
      learnOverlap wheat nirnroot S.empty
      expectThrows @InconsistentOverlap $
        learnOverlap wheat nirnroot $ S.fromList [restoreHealth]

    it "cannot learn overlap with an effect an ingredient is known not to have" $
      runAlchemyData $ do
      learnOverlap blueMountainFlower butterflyWing $
        S.fromList [restoreHealth]
      learnOverlap blueMountainFlower wheat $
        S.fromList [restoreHealth, fortifyHealth]

      -- Butterfly Wing is known not to have Fortify Health at this point
      expectThrows @InconsistentOverlap $
        learnOverlap butterflyWing wheat $
          S.fromList [restoreHealth, fortifyHealth]

    it "cannot learn overlap that doesn't contain common effects" $
      runAlchemyData $ do
      learnOverlap blueMountainFlower butterflyWing $
        S.fromList [restoreHealth]
      learnOverlap blueMountainFlower wheat $
        S.fromList [restoreHealth, fortifyHealth]

      -- Both have Restore Health, so overlap must include it
      expectThrows @InconsistentOverlap $
        learnOverlap butterflyWing wheat S.empty



--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------


runAlchemyData
  :: StateC AlchemyData (
     ErrorC InconsistentOverlap (
     ErrorC InconsistentEffect (
     LiftC IO))) a
  -> IO ()
runAlchemyData =
  runM @IO .
  expectingNoErrors @InconsistentEffect .
  expectingNoErrors @InconsistentOverlap .
  void . execState emptyAlchemyData


expectingNoErrors
  :: ( Show e, Monad m )
  => ErrorC e m a
  -> m a
expectingNoErrors = flip catching (error.show)


shouldReturn
  :: ( HasCallStack
     , Show a
     , Eq a
     , Has (Lift IO) sig m )
  => m a -> a -> m ()
action `shouldReturn` expected = do
  result <- action
  sendIO $ result `shouldBe` expected

shouldReturnSatisfying
  :: ( HasCallStack
     , Show a
     , Eq a
     , Has (Lift IO) sig m )
  => m a
  -> (a -> Bool)
  -> m ()
action `shouldReturnSatisfying` predicate = do
  result <- action
  sendIO $ result `shouldSatisfy` predicate

expectThrows
  :: forall e m a.
     Monad m
  => ErrorC e m a
  -> m ()
expectThrows action = runError action >>= \case
  Left _  -> return ()
  Right _ -> error "Expected an error"
