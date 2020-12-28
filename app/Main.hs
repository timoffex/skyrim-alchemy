{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

import qualified AlchemyData                  as AD
import           BronKerbosch
    ( bronKerbosch )
import           Control.Carrier.Error.Either
    ( runError )
import           Control.Carrier.Lift
    ( Has, runM )
import           Control.Carrier.State.Strict
    ( evalState, execState, run )
import           Control.Effect.Error
    ( Error, throwError )
import           Control.Effect.Lift
    ( Lift, sendIO )
import           Control.Effect.State
    ( State, get, gets, modify )
import           Control.Exception
    ( SomeException, catch )
import           Control.Monad
    ( forM, forever, unless, void, when )
import           Data.Foldable
    ( fold, forM_, maximumBy, minimumBy )
import           Data.Function
    ( fix, on )
import qualified Data.Map.Strict              as M
import qualified Data.Set                     as S
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as LT
import           Data.Void
    ( Void )
import           System.Console.Readline
    ( addHistory, readline )
import           System.Exit
    ( exitSuccess )
import qualified Text.Megaparsec              as MP
import qualified Text.Megaparsec.Char         as MP
import qualified Text.Megaparsec.Char.Lexer   as L

main :: IO ()
main = do
  let inputFile = "output.txt"
      outputFile = inputFile

  -- Use lazy text for parsing the file to be able to support very
  -- large files without loading them completely into memory. I
  -- haven't benchmarked this, and it's 100% a premature optimization,
  -- but it makes me happy.
  inputFileContents <- LT.pack <$>
    catch @SomeException (readFile inputFile) (return . const "")
  case MP.parse ingredientFile inputFile inputFileContents of
    Left bundle       -> putStr (MP.errorBundlePretty bundle)
    Right initialData -> do
      putStrLn "Parsed successfully!"

      runM . evalState initialData $ forever $ do
        -- Save before every command.
        get >>=
          sendIO .
          writeFile outputFile .
          serializeAlchemyData

        sendIO tryReadCommand >>= \case
          Nothing -> sendIO $ putStrLn "Invalid command."
          Just cmd -> runCommand cmd


--------------------------------------------------------------------------------
-- Stateful AlchemyData computations
--------------------------------------------------------------------------------

listAllEffects
  :: Has (State AD.AlchemyData) sig m
  => m (S.Set AD.EffectName)
listAllEffects = gets AD.allKnownEffects

listAllIngredients
  :: Has (State AD.AlchemyData) sig m
  => m (S.Set AD.IngredientName)
listAllIngredients = gets AD.allKnownIngredients


listIngredientsWith
  :: Has (State AD.AlchemyData) sig m
  => AD.EffectName
  -> m (S.Set AD.IngredientName)
listIngredientsWith = listIngredientsWithAnyOf . S.singleton

listIngredientsWithAnyOf
  :: Has (State AD.AlchemyData) sig m
  => S.Set AD.EffectName
  -> m (S.Set AD.IngredientName)
listIngredientsWithAnyOf = gets . AD.ingredientsWithAnyOf

listEffectsOf
  :: Has (State AD.AlchemyData) sig m
  => AD.IngredientName
  -> m (S.Set AD.EffectName)
listEffectsOf = gets . AD.effectsOf

listNonEffectsOf
  :: Has (State AD.AlchemyData) sig m
  => AD.IngredientName
  -> m (S.Set AD.EffectName)
listNonEffectsOf = gets . AD.nonEffectsOf

listPotentialEffectsOf
  :: Has (State AD.AlchemyData) sig m
  => AD.IngredientName
  -> m (S.Set AD.EffectName)
listPotentialEffectsOf ing =
  S.difference <$> listAllEffects <*> listNonEffectsOf ing

listPotentialNewEffectsOf
  :: Has (State AD.AlchemyData) sig m
  => AD.IngredientName
  -> m (S.Set AD.EffectName)
listPotentialNewEffectsOf ing =
  S.difference <$> listPotentialEffectsOf ing <*> listEffectsOf ing

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
  potentialEffs <- listPotentialNewEffectsOf ing
  ingsToConsider <- listIngredientsWithAnyOf potentialEffs
  cliques <- bronKerbosch <$>
    getNonoverlapAdjacencyMap potentialEffs ingsToConsider

  covers <- forM cliques $ \clique -> do
    cliqueEffs <- S.unions <$> mapM listEffectsOf (S.toList clique)
    let effsMissingFromClique = S.difference potentialEffs cliqueEffs

    extraIngs <- execState (S.empty :: S.Set AD.IngredientName) $ fix $ \loop -> do
      ingsSoFar <- get
      effsSoFar <- fold <$> mapM listEffectsOf (S.toList ingsSoFar)
      let missingEffs = effsMissingFromClique `S.difference` effsSoFar

      unless (S.null missingEffs) $ do
        ingsForMissingEffs <- listIngredientsWithAnyOf missingEffs

        rankedIngs <- forM (S.toList ingsForMissingEffs) $ \ingToRank -> do
          ingEffs <- listEffectsOf ingToRank
          return (ingToRank, S.size $ S.intersection ingEffs missingEffs)

        modify $ S.insert $
          fst $ maximumBy (compare `on` snd) rankedIngs

        loop


    return $ EffectCover
      { coverClique = clique
      , coverFilling = extraIngs }

  return $ minimumBy (compare `on` coverSize) covers


listMaximalCliques
  :: Has (State AD.AlchemyData) sig m
  => m [S.Set AD.IngredientName]
listMaximalCliques = bronKerbosch <$> do
  alch <- get

  let overlaps = AD.allKnownOverlaps alch

  -- TODO: Share code with getNonoverlapAdjacencyMap

  -- Construct an adjacency map from the overlaps where two
  -- ingredients are adjacent iff they have an empty overlap.
  return $ run $ execState M.empty $
    forM_ overlaps $ \((ing1, ing2), effs) ->
      when (S.null effs) $ do
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
    forM_ overlaps $ \((ing1, ing2), effs) ->
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
  runError @AD.InconsistentOverlap (AD.learnOverlap ing1 ing2 effs) >>= \case
    Left err -> throwError $ T.pack $ show err
    Right () -> return ()



--------------------------------------------------------------------------------
-- Parser helpers
--------------------------------------------------------------------------------

-- Use lazy text for parsing.
type Parser = MP.Parsec Void LT.Text

sc :: Parser ()
sc = L.space
  MP.space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = fmap LT.toStrict . L.symbol sc . LT.fromStrict

--------------------------------------------------------------------------------
-- Parsing an ingredient save file
--------------------------------------------------------------------------------

-- TODO: Layer StateC AD.AlchemyData on top of the parser and report
-- overlap problems with a line number. I'll need an Algebra instance
-- for MP.ParseT for the Lift effect.
ingredientFile :: Parser AD.AlchemyData
ingredientFile = do
  ings <- MP.many ingredientDef
  overlaps <- MP.many overlapDef
  MP.eof

  let alch = run .
             runError @AD.InconsistentOverlap .
             runError @AD.InconsistentEffect .
             execState AD.emptyAlchemyData $ do
        forM_ ings $ \(IngredientDef ingName effs) -> do
          AD.learnIngredient ingName
          mapM_ (AD.learnIngredientEffect ingName) effs

        forM_ overlaps $ \(OverlapDef ing1 ing2 effs) -> do
          AD.learnOverlap ing1 ing2 effs

  case alch of
    Left err              -> fail $ show err
    Right (Left err)      -> fail $ show err
    Right (Right success) -> return success


data IngredientDef
  = IngredientDef AD.IngredientName (S.Set AD.EffectName)
data OverlapDef
  = OverlapDef AD.IngredientName AD.IngredientName (S.Set AD.EffectName)



ingredientDef :: Parser IngredientDef
ingredientDef = lexeme $ do
  void (symbol "!ING")
  ingName <- ingredientName
  void (symbol ":")
  effNames <- effectName `MP.sepEndBy` symbol ","

  return $ IngredientDef ingName (S.fromList effNames)


namePart :: Parser T.Text
namePart = T.pack <$> MP.some (MP.alphaNumChar MP.<|> MP.char '\'')

ingredientName :: Parser AD.IngredientName
ingredientName = AD.ingredientName . T.unwords <$>
  namePart `MP.sepEndBy1` MP.space1

effectName :: Parser AD.EffectName
effectName = AD.effectName . T.unwords <$>
  namePart `MP.sepEndBy1` MP.space1

overlapDef :: Parser OverlapDef
overlapDef = lexeme $ do
  void (symbol "!OVERLAP")

  ingName1 <- ingredientName
  void (symbol ",")
  ingName2 <- ingredientName
  void (symbol ":")
  effNames <- effectName `MP.sepEndBy` symbol ","

  return $ OverlapDef ingName1 ingName2 (S.fromList effNames)

--------------------------------------------------------------------------------
-- Saving an AlchemyData to a save file
--------------------------------------------------------------------------------

serializeAlchemyData :: AD.AlchemyData -> String
serializeAlchemyData alchemyData = run . execState "" $ do
  let
    append s = modify (++ s)
    newline = append "\n"

  -- Print all known ingredients
  forM_ (AD.allKnownIngredients alchemyData) $ \ing -> do
    append "!ING "
    append $ show ing
    append ": "
    forM_ (AD.effectsOf ing alchemyData) $ \eff -> do
      append $ show eff
      append ", "
    newline

  -- Print all known overlaps
  forM_ (AD.allKnownOverlaps alchemyData) $ \((ing1, ing2), effs) -> do
    append "!OVERLAP "
    append $ show ing1
    append ", "
    append $ show ing2
    append ": "
    forM_ effs $ \eff -> do
      append $ show eff
      append ", "
    newline


--------------------------------------------------------------------------------
-- Command-line interface
--------------------------------------------------------------------------------

data Command
  = LearnOverlap AD.IngredientName AD.IngredientName (S.Set AD.EffectName)
  | ListEffects
  | ListEffectsOf AD.IngredientName
  | ListNonEffectsOf AD.IngredientName
  | ListPotentialEffectsOf AD.IngredientName
  | SuggestCombineWith AD.IngredientName
  | ListIngredients
  | ListMaximalCliques
  | Exit

----------------------------------------
-- Parsing
----------------------------------------

tryReadCommand :: IO (Maybe Command)
tryReadCommand = do
  readline ">>> " >>= \case
    Nothing -> return $ Just Exit
    Just s -> do
      addHistory s
      case MP.parse commandDef "input" (LT.pack s) of
        Left err  -> do
          putStr (MP.errorBundlePretty err)
          return Nothing
        Right cmd -> do
          return $ Just cmd

commandDef :: Parser Command
commandDef = MP.choice
  [ learnOverlapCommand
  , listEffectsOfCommand
  , listNonEffectsOfCommand
  , listPotentialEffectsOfCommand
  , suggestCombineWithCommand
  , listEffectsCommand
  , listIngredientsCommand
  , listMaximalCliquesCommand
  , exitCommand ]

learnOverlapCommand :: Parser Command
learnOverlapCommand = do
  void (symbol "overlap")
  ingName1 <- ingredientName
  void (symbol ",")
  ingName2 <- ingredientName
  void (symbol ":")
  effs <- effectName `MP.sepEndBy` symbol ","
  return $ LearnOverlap ingName1 ingName2 (S.fromList effs)

listEffectsCommand :: Parser Command
listEffectsCommand =
  symbol "effects" >> MP.eof >> return ListEffects

listIngredientsCommand :: Parser Command
listIngredientsCommand =
  symbol "ingredients" >> MP.eof >> return ListIngredients

listEffectsOfCommand :: Parser Command
listEffectsOfCommand = do
  void (symbol "effects of")
  ListEffectsOf <$> ingredientName

listNonEffectsOfCommand :: Parser Command
listNonEffectsOfCommand = do
  void (symbol "noneffects" >> MP.optional (symbol "of"))
  ListNonEffectsOf <$> ingredientName

listPotentialEffectsOfCommand :: Parser Command
listPotentialEffectsOfCommand = do
  void (symbol "potential effects" >> MP.optional (symbol "of"))
  ListPotentialEffectsOf <$> ingredientName

suggestCombineWithCommand :: Parser Command
suggestCombineWithCommand = do
  void (symbol "suggestions" >> MP.optional (symbol "for"))
  SuggestCombineWith <$> ingredientName

listMaximalCliquesCommand :: Parser Command
listMaximalCliquesCommand =
  symbol "cliques" >> MP.eof >> return ListMaximalCliques

exitCommand :: Parser Command
exitCommand = symbol "exit" >> return Exit

----------------------------------------
-- Interpreting
----------------------------------------

runCommand
  :: ( Has (Lift IO) sig m
     , Has (State AD.AlchemyData) sig m )
  => Command
  -> m ()
runCommand = \case
  Exit                 -> sendIO exitSuccess
  ListEffects          -> listAllEffects >>= mapM_ (sendIO . print)
  ListEffectsOf ing    -> listEffectsOf ing >>= mapM_ (sendIO . print)
  ListNonEffectsOf ing -> listNonEffectsOf ing >>= mapM_ (sendIO . print)
  ListIngredients      -> listAllIngredients >>= mapM_ (sendIO . print)
  ListPotentialEffectsOf ing ->
    listPotentialNewEffectsOf ing >>= mapM_ (sendIO . print)
  SuggestCombineWith ing -> do
    cover <- minimumPotentialEffectsCoverOf ing
    sendIO $ putStrLn "Cover clique:"
    forM_ (coverClique cover) $ \eff ->
      sendIO $ putStrLn $ "  " ++ show eff
    sendIO $ putStrLn "Cover filling:"
    forM_ (coverFilling cover) $ \eff ->
      sendIO $ putStrLn $ "  " ++ show eff
  ListMaximalCliques   -> do
    cliques <- listMaximalCliques
    forM_ cliques $ \clique -> do
      sendIO $ putStrLn "Clique:"
      mapM_ (sendIO . print) clique
      sendIO $ putStrLn ""
  LearnOverlap ing1 ing2 effs ->
    runError (tryLearnOverlap ing1 ing2 effs) >>= \case
      Left err -> sendIO $ putStrLn $ "Error: " ++ T.unpack err
      Right () -> return ()



--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------


multiMapInsert
  :: ( Ord k, Ord a )
  => k -> a -> M.Map k (S.Set a) -> M.Map k (S.Set a)
multiMapInsert k a = M.insertWith (<>) k (S.singleton a)
