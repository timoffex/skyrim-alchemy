{-# LANGUAGE ConstraintKinds  #-}
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
    ( Foldable (fold), maximumBy, minimumBy )
import           Data.Function
    ( fix, on )
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
  sendIO $ putStrLn "Cover clique:"
  printOnSeparateLinesWithPrefix "  " $ coverClique cover
  sendIO $ putStrLn "Cover filling:"
  printOnSeparateLinesWithPrefix "  " $ coverFilling cover


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


data EffectCover
  = EffectCover
    { coverClique  :: S.Set AD.IngredientName
    , coverFilling :: S.Set AD.IngredientName }

coverSize :: EffectCover -> Int
coverSize c = S.size (coverClique c) + S.size (coverFilling c)

-- | Approximates a minimum cover of the potential unknown effects on
-- the ingredient.
minimumPotentialEffectsCoverOf
  :: Has (State AD.AlchemyData) sig m
  => AD.IngredientName
  -> m EffectCover
minimumPotentialEffectsCoverOf ing = do
  -- Get the list of potential effects. Consider all ingredients that
  -- contain at least one of these effects. Use Bron-Kerbosch to list
  -- all maximal cliques of these ingredients. For each clique, fill
  -- in the missing effects using ingredients from the list. Return
  -- the smallest result.
  potentialEffs <- pleaseListPotentialEffectsOf ing
  ingsToConsider <- pleaseListIngredientsWithAnyOf potentialEffs
  cliques <- bronKerbosch <$>
    getNonoverlapAdjacencyMap potentialEffs ingsToConsider

  covers <- forM cliques $ \clique -> do
    cliqueEffs <- S.unions <$> mapM pleaseListEffectsOf (S.toList clique)
    let effsMissingFromClique = S.difference potentialEffs cliqueEffs

    extraIngs <- execState (S.empty @AD.IngredientName) $ fix $ \loop -> do
      ingsSoFar <- get
      effsSoFar <- fold <$> mapM pleaseListEffectsOf (S.toList ingsSoFar)
      let missingEffs = effsMissingFromClique `S.difference` effsSoFar

      unless (S.null missingEffs) $ do
        ingsForMissingEffs <- pleaseListIngredientsWithAnyOf missingEffs

        rankedIngs <- forM (S.toList ingsForMissingEffs) $ \ingToRank -> do
          ingEffs <- pleaseListEffectsOf ingToRank
          return (ingToRank, S.size $ S.intersection ingEffs missingEffs)

        -- TODO: Partial
        modify $ S.insert $
          fst $ maximumBy (compare `on` snd) rankedIngs

        loop


    return $ EffectCover
      { coverClique = clique
      , coverFilling = extraIngs }

  -- TODO: Partial
  return $ minimumBy (compare `on` coverSize) covers


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

  -- TODO: Share code with getNonoverlapAdjacencyMap

  -- Construct an adjacency map from the overlaps where two
  -- ingredients are adjacent iff they have an empty overlap.
  return $ run $ execState M.empty $
    forM_ overlaps $ \(ingPair, effs) ->
      when (S.null effs) $ do
        let (ing1, ing2) = unpair ingPair
        modify $ multiMapInsert ing1 ing2
        modify $ multiMapInsert ing2 ing1

getNonoverlapAdjacencyMap
  :: Has (State AD.AlchemyData) sig m
  => S.Set AD.EffectName
  -> S.Set AD.IngredientName
  -> m (M.Map AD.IngredientName (S.Set AD.IngredientName))
getNonoverlapAdjacencyMap effsThatMatter ings = do
  overlaps <- gets AD.allKnownOverlaps

  -- TODO: Do this more efficiently!

  -- Construct an adjacency map from the overlaps where two
  -- ingredients are adjacent iff they have an empty overlap.
  return $ run $ execState M.empty $
    forM_ overlaps $ \(ingPair, effs) -> do
      let (ing1, ing2) = unpair ingPair
      when (S.member ing1 ings &&
            S.member ing2 ings &&
            effs `S.isSubsetOf` effsThatMatter) $ do
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
