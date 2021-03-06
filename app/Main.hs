{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

import qualified AlchemyData                  as AD
import qualified Command                      as Cmd
import           Control.Carrier.Error.Either
    ( runError )
import           Control.Carrier.Lift
    ( runM )
import           Control.Carrier.State.Strict
    ( evalState, execState, run )
import           Control.Effect.Lift
    ( sendIO )
import           Control.Effect.State
    ( get, modify )
import           Control.Exception
    ( SomeException, catch )
import           Control.Monad
    ( forever, void )
import           Data.Foldable
    ( forM_ )
import qualified Data.Set                     as S
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as LT
import           Data.UPair
    ( unpair )
import           Data.Void
    ( Void )
import           System.Console.Readline
    ( addHistory, readline )
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
          Just cmd -> Cmd.runCommand cmd


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
namePart = T.pack <$>
  MP.some (MP.alphaNumChar MP.<|> MP.char '\'' MP.<|> MP.char '-')

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
  forM_ (AD.allKnownOverlaps alchemyData) $ \(ingPair, effs) -> do
    let (ing1, ing2) = unpair ingPair
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
-- Parsing commands
--------------------------------------------------------------------------------

tryReadCommand :: IO (Maybe Cmd.Command)
tryReadCommand = do
  readline ">>> " >>= \case
    Nothing -> return $ Just Cmd.exit
    Just s -> do
      addHistory s
      case MP.parse commandDef "input" (LT.pack s) of
        Left err  -> do
          putStr (MP.errorBundlePretty err)
          return Nothing
        Right cmd -> do
          return $ Just cmd

commandDef :: Parser Cmd.Command
commandDef = MP.choice
  [ learnOverlapCommand
  , learnIngredientEffectCommand
  , listEffectsOfCommand
  , listNonEffectsOfCommand
  , listPotentialEffectsOfCommand
  , suggestCombineWithCommand
  , listEffectsCommand
  , listIngredientsWithAllOfCommand
  , listIngredientsCommand
  , listMaximalCliquesCommand
  , exitCommand ]

learnOverlapCommand :: Parser Cmd.Command
learnOverlapCommand = do
  void (symbol "overlap")
  ingName1 <- ingredientName
  void (symbol ",")
  ingName2 <- ingredientName
  void (symbol ":")
  effs <- effectName `MP.sepEndBy` symbol ","
  return $ Cmd.learnOverlap ingName1 ingName2 (S.fromList effs)

learnIngredientEffectCommand :: Parser Cmd.Command
learnIngredientEffectCommand = do
  void (symbol "learn effect")
  ingName <- ingredientName
  void (symbol ":")
  effs <- effectName `MP.sepEndBy1` symbol ","
  return $ Cmd.learnIngredientEffect ingName (S.fromList effs)

listEffectsCommand :: Parser Cmd.Command
listEffectsCommand =
  symbol "effects" >> MP.eof >> return Cmd.listEffects

listIngredientsCommand :: Parser Cmd.Command
listIngredientsCommand =
  symbol "ingredients" >> MP.eof >> return Cmd.listIngredients

listIngredientsWithAllOfCommand :: Parser Cmd.Command
listIngredientsWithAllOfCommand = do
  void (symbol "ingredients with")
  effs <- effectName `MP.sepEndBy1` symbol ","
  return $ Cmd.listIngredientsWithAllOf (S.fromList effs)

listEffectsOfCommand :: Parser Cmd.Command
listEffectsOfCommand = do
  void (symbol "effects of")
  Cmd.listEffectsOf <$> ingredientName

listNonEffectsOfCommand :: Parser Cmd.Command
listNonEffectsOfCommand = do
  void (symbol "noneffects" >> MP.optional (symbol "of"))
  Cmd.listNonEffectsOf <$> ingredientName

listPotentialEffectsOfCommand :: Parser Cmd.Command
listPotentialEffectsOfCommand = do
  void (symbol "potential effects" >> MP.optional (symbol "of"))
  Cmd.listPotentialEffectsOf <$> ingredientName

suggestCombineWithCommand :: Parser Cmd.Command
suggestCombineWithCommand = do
  void (symbol "suggestions" >> MP.optional (symbol "for"))
  Cmd.suggestCombineWith <$> ingredientName

listMaximalCliquesCommand :: Parser Cmd.Command
listMaximalCliquesCommand =
  symbol "cliques" >> MP.eof >> return Cmd.listMaximalCliques

exitCommand :: Parser Cmd.Command
exitCommand = symbol "exit" >> return Cmd.exit
