{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

import qualified AlchemyData                  as AD
import           Control.Carrier.Lift
    ( Has )
import           Control.Carrier.State.Strict
import           Control.Carrier.Throw.Either
import           Control.Effect.State
    ( State, get, gets, modify, put )
import           Control.Effect.Throw
    ( Throw, throwError )
import           Control.Monad
    ( void )
import           Control.Monad.Trans.Class
import           Data.Foldable
    ( forM_ )
import           Data.List
import qualified Data.Set                     as S
import           Data.Void
    ( Void )
import           System.Environment
import           System.IO
import qualified Text.Megaparsec              as MP
import qualified Text.Megaparsec.Char         as MP
import qualified Text.Megaparsec.Char.Lexer   as L

main :: IO ()
main = do
  -- Use LineBuffering to handle backspaces like the user expects
  hSetBuffering stdin LineBuffering

  [inputFile] <- getArgs
  !inputFileContents <- readFile inputFile

  case MP.parse ingredientFile inputFile inputFileContents of
    Left bundle       -> putStr (MP.errorBundlePretty bundle)
    Right initialData -> do
      putStrLn "Parsed successfully!"
      writeFile "output.txt" $ serializeAlchemyData initialData


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

learnIngredientEffect
  :: Has (State AD.AlchemyData) sig m
  => AD.IngredientName
  -> AD.EffectName
  -> m ()
learnIngredientEffect ing eff = modify $ AD.learnIngredientEffect ing eff

tryLearnOverlap
  :: ( Has (State AD.AlchemyData) sig m
     , Has (Throw String) sig m )
  => AD.IngredientName
  -> AD.IngredientName
  -> S.Set AD.EffectName
  -> m ()
tryLearnOverlap ing1 ing2 effs = do
  alch <- get
  case AD.learnOverlap ing1 ing2 effs alch of
    Left err    -> throwError $ show err
    Right alch' -> put alch'


--------------------------------------------------------------------------------
-- Parsing an ingredient save file
--------------------------------------------------------------------------------


type Parser = MP.Parsec Void String

sc :: Parser ()
sc = L.space
  MP.space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc


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
        forM_ ings $ \(IngredientDef ingName effs) ->
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


namePart :: Parser String
namePart = MP.some (MP.alphaNumChar MP.<|> MP.char '\'')

ingredientName :: Parser AD.IngredientName
ingredientName = AD.IngredientName . unwords <$>
  namePart `MP.sepEndBy1` MP.space1

effectName :: Parser AD.EffectName
effectName = AD.EffectName . unwords <$>
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
