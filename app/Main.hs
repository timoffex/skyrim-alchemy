{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}


import qualified AlchemyData                  as AD
import           AlchemyInteraction
import           AlchemyInteractionIO
    ( runAlchemyInteractionIO )
import           CLI
    ( parseCommand, runCommand )
import qualified Command                      as Cmd
import           Control.Algebra
import           Control.Carrier.Error.Either
    ( runError )
import           Control.Carrier.Lift
    ( runM )
import           Control.Carrier.State.Strict
    ( evalState, execState, run )
import           Control.Effect.Lift
    ( sendIO )
import           Control.Effect.Lift
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

      runM .
          evalState initialData .
          runAlchemyInteractionIO .
          forever $ do
        -- Save before every command.
        get >>=
          sendIO .
          writeFile outputFile .
          serializeAlchemyData

        tryReadCommand >>= \case
          Nothing -> sendIO $ putStrLn "Invalid command."
          Just cmd -> runCommand cmd


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

tryReadCommand
  :: ( Has AlchemyInteraction sig m
     , Has (Lift IO) sig m
     )
  => m (Maybe Cmd.Command)
tryReadCommand = do
  sendIO (readline ">>> ") >>= \case
    Nothing -> return $ Just Cmd.exit
    Just s -> do
      sendIO $ addHistory s
      parseCommand s
