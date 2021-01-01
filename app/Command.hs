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
  , listMaximalCliques
  , learnOverlap
  , learnIngredientEffect
  ) where


import qualified AlchemyData                  as AD
import           BronKerbosch
    ( bronKerbosch )
import           Control.Algebra
    ( Has, run )
import           Control.Carrier.Error.Extra
    ( catching, rethrowing )
import           Control.Carrier.State.Strict
    ( State, execState, get, gets, modify )
import           Control.Effect.Error
    ( Error )
import           Control.Effect.Lift
    ( Lift, sendIO )
import           Control.Effect.State ()
import           Control.Monad
    ( forM, forM_, unless, when )
import           Data.Foldable
    ( maximumBy )
import           Data.Function
    ( fix, on )
import           Data.List
    ( intercalate )
import qualified Data.Map.Strict              as M
import qualified Data.Set                     as S
import qualified Data.Text                    as T
import           Data.UPair
    ( unpair )
import           System.Exit
    ( exitSuccess )


-- | A command for interacting with an 'AD.AlchemyData'.
newtype Command
  = Command
    { runCommand :: forall m sig. HasCommandEffects sig m => m () }


type HasCommandEffects sig m =
  ( Has (Lift IO) sig m
  , Has (State AD.AlchemyData) sig m )


-- TODO: Use an Either/Maybe effect to short-circuit.
-- | Exits the program.
exit :: Command
exit = Command $ sendIO exitSuccess


--------------------------------------------------------------------------------
-- Queries returning effects
--------------------------------------------------------------------------------

listEffects :: Command
listEffects = Command $
  pleaseListEffects >>= printOnSeparateLines

listEffectsOf :: AD.IngredientName -> Command
listEffectsOf ing = Command $
  pleaseListEffectsOf ing >>= printOnSeparateLines

listNonEffectsOf :: AD.IngredientName -> Command
listNonEffectsOf ing = Command $
  pleaseListNonEffectsOf ing >>= printOnSeparateLines

listPotentialEffectsOf :: AD.IngredientName -> Command
listPotentialEffectsOf ing = Command $
  pleaseListPotentialEffectsOf ing >>= printOnSeparateLines


--------------------------------------------------------------------------------
-- Queries returning ingredients
--------------------------------------------------------------------------------

listIngredients :: Command
listIngredients = Command $
  pleaseListIngredients >>= printOnSeparateLines

listIngredientsWithAllOf :: S.Set AD.EffectName -> Command
listIngredientsWithAllOf effs = Command $
  pleaseListIngredientsWithAllOf effs >>= printOnSeparateLines

suggestCombineWith :: AD.IngredientName -> Command
suggestCombineWith ing = Command $ do
  cover <- minimumPotentialEffectsCoverOf ing
  printOnSeparateLines $ coverIngsAndEffs cover


listMaximalCliques :: Command
listMaximalCliques = Command $ do
  cliques <- pleaseListMaximalCliques
  forM_ cliques $ \clique -> do
    sendIO $ putStrLn "Clique:"
    printOnSeparateLines clique
    sendIO $ putStrLn ""


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
    sendIO $ putStrLn $ "Error: " ++ T.unpack err

learnIngredientEffect
  :: AD.IngredientName
  -> S.Set AD.EffectName
  -> Command
learnIngredientEffect ing effs = Command $
  catching (mapM_ (tryLearnIngredientEffect ing) effs) $ \err ->
    sendIO $ putStrLn $ "Error: " ++ T.unpack err




--------------------------------------------------------------------------------
-- Command implementation helpers
--------------------------------------------------------------------------------


newtype EffectCover
  = EffectCover { coverIngsAndEffs :: [IngredientWithSignificantEffects] }

data IngredientWithSignificantEffects
  = IngredientWithSignificantEffects
    { ingredient :: AD.IngredientName
    , effects    :: S.Set AD.EffectName }
  deriving ( Eq, Ord )

instance Show IngredientWithSignificantEffects where
  show x = show (ingredient x) ++
    " for " ++
    intercalate ", " (show <$> S.toList (effects x))

-- | Approximates a minimum cover of the potential unknown effects on
-- the ingredient.
minimumPotentialEffectsCoverOf
  :: Has (State AD.AlchemyData) sig m
  => AD.IngredientName
  -> m EffectCover
minimumPotentialEffectsCoverOf ing = do

  -- Approximate a minimum cover by using the greedy algorithm: at
  -- each step, pick the ingredient that contains the largest number
  -- of effects that are still missing.
  potentialEffs <- pleaseListPotentialEffectsOf ing

  ings <- execState ([] @IngredientWithSignificantEffects) $
    fix $ \loop -> do
    ingsSoFar <- get @[IngredientWithSignificantEffects]
    let
      effsSoFar = S.unions (effects <$> ingsSoFar)
      missingEffs = potentialEffs `S.difference` effsSoFar

    unless (S.null missingEffs) $ do
      ingsForMissingEffs <- pleaseListIngredientsWithAnyOf missingEffs

      rankedIngsWithEffs <- forM (S.toList ingsForMissingEffs) $
        \ingToRank -> do
        ingEffs <- pleaseListEffectsOf ingToRank
        let importantEffs = S.intersection ingEffs missingEffs
        return
          ( IngredientWithSignificantEffects
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

  return $ EffectCover $ reverse ings

pleaseListEffects
  :: Has (State AD.AlchemyData) sig m
  => m (S.Set AD.EffectName)
pleaseListEffects = gets AD.allKnownEffects

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
pleaseListEffectsOf = gets . AD.effectsOf

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

pleaseListMaximalCliques
  :: Has (State AD.AlchemyData) sig m
  => m [S.Set AD.IngredientName]
pleaseListMaximalCliques = bronKerbosch <$> do
  alch <- get

  let overlaps = AD.allKnownOverlaps alch

  -- Construct an adjacency map from the overlaps where two
  -- ingredients are adjacent iff they have an empty overlap.
  return $ run $ execState M.empty $
    forM_ overlaps $ \(ingPair, effs) ->
      when (S.null effs) $ do
        let (ing1, ing2) = unpair ingPair
        modify $ multiMapInsert ing1 ing2
        modify $ multiMapInsert ing2 ing1


tryLearnOverlap
  :: ( Has (State AD.AlchemyData) sig m
     , Has (Error T.Text) sig m )
  => AD.IngredientName
  -> AD.IngredientName
  -> S.Set AD.EffectName
  -> m ()
tryLearnOverlap ing1 ing2 effs =
  rethrowing @AD.InconsistentOverlap (T.pack . show) $
  AD.learnOverlap ing1 ing2 effs


tryLearnIngredientEffect
  :: ( Has (State AD.AlchemyData) sig m
     , Has (Error T.Text) sig m )
  => AD.IngredientName
  -> AD.EffectName
  -> m ()
tryLearnIngredientEffect ing eff =
  rethrowing @AD.InconsistentEffect (T.pack . show) $
  AD.learnIngredientEffect ing eff


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

printOnSeparateLines
  :: ( Has (Lift IO) sig m
     , Foldable t
     , Show a )
  => t a -> m ()
printOnSeparateLines = printOnSeparateLinesWithPrefix ""

printOnSeparateLinesWithPrefix
  :: ( Has (Lift IO) sig m
     , Foldable t
     , Show a )
  => String -> t a -> m ()
printOnSeparateLinesWithPrefix prefix = mapM_ $ \a ->
  sendIO $ putStrLn $ prefix ++ show a

multiMapInsert
  :: ( Ord k, Ord a )
  => k -> a -> M.Map k (S.Set a) -> M.Map k (S.Set a)
multiMapInsert k a = M.insertWith (<>) k (S.singleton a)
