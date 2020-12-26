{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

import qualified AlchemyData                  as AD
import           BronKerbosch
    ( bronKerbosch )
import           Control.Carrier.Lift
    ( Has, runM )
import           Control.Carrier.State.Strict
    ( evalState, execState, run )
import           Control.Carrier.Throw.Either
    ( liftEither, runThrow )
import           Control.Effect.Lift
    ( Lift, sendIO )
import           Control.Effect.State
    ( State, get, gets, modify, put )
import           Control.Effect.Throw
    ( Throw, throwError )
import           Control.Monad
    ( forever, void, when )
import           Data.Foldable
    ( forM_ )
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
  inputFileContents <- LT.pack <$> readFile inputFile
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
listPotentialEffectsOf ing = do
  allEffs <- listAllEffects
  nonEffs <- listNonEffectsOf ing
  return $ S.difference allEffs nonEffs

listMaximalCliques
  :: Has (State AD.AlchemyData) sig m
  => m [S.Set AD.IngredientName]
listMaximalCliques = bronKerbosch <$> do
  alch <- get

  let overlaps = AD.allKnownOverlaps alch

  -- Construct an adjacency map from the overlaps where two
  -- ingredients are adjacent iff they have an empty overlap.
  return $ run $ execState M.empty $
    forM_ overlaps $ \((ing1, ing2), effs) ->
      when (S.null effs) $ do
        modify $ multiMapInsert ing1 ing2
        modify $ multiMapInsert ing2 ing1

tryLearnOverlap
  :: ( Has (State AD.AlchemyData) sig m
     , Has (Throw T.Text) sig m )
  => AD.IngredientName
  -> AD.IngredientName
  -> S.Set AD.EffectName
  -> m ()
tryLearnOverlap ing1 ing2 effs = do
  alch <- get
  case AD.learnOverlap ing1 ing2 effs alch of
    Left err    -> throwError $ T.pack $ show err
    Right alch' -> put alch'


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
             runThrow @AD.InconsistentOverlap .
             execState AD.emptyAlchemyData $ do
        forM_ ings $ \(IngredientDef ingName effs) -> do
          modify $ AD.learnIngredient ingName
          forM_ effs $ \eff ->
            modify $ AD.learnIngredientEffect ingName eff

        forM_ overlaps $ \(OverlapDef ing1 ing2 effs) -> do
          alch0 <- get
          alch1 <- liftEither $ AD.learnOverlap ing1 ing2 effs alch0
          put alch1

  case alch of
    Left err      -> fail $ show err
    Right success -> return success


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
    listPotentialEffectsOf ing >>= mapM_ (sendIO . print)
  ListMaximalCliques   -> do
    cliques <- listMaximalCliques
    forM_ cliques $ \clique -> do
      sendIO $ putStrLn "Clique:"
      mapM_ (sendIO . print) clique
      sendIO $ putStrLn ""
  LearnOverlap ing1 ing2 effs ->
    runThrow (tryLearnOverlap ing1 ing2 effs) >>= \case
      Left err -> sendIO $ putStrLn $ "Error: " ++ T.unpack err
      Right () -> return ()



--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------


multiMapInsert
  :: ( Ord k, Ord a )
  => k -> a -> M.Map k (S.Set a) -> M.Map k (S.Set a)
multiMapInsert k a = M.insertWith (<>) k (S.singleton a)
