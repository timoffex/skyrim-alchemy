{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Defines the command-line interface to this program.
module CLI
  ( tryReadCommand
  , Cmd.runCommand
  , Cmd.Command
  ) where


import qualified AlchemyData                as AD
import qualified Command                    as Cmd
import           Data.Functor
    ( void )
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import           Data.Void
    ( Void )
import           System.Console.Readline
    ( addHistory, readline )
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MP
import qualified Text.Megaparsec.Char.Lexer as L


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

symbol :: T.Text -> Parser T.Text
symbol = fmap LT.toStrict . L.symbol sc . LT.fromStrict


namePart :: Parser T.Text
namePart = T.pack <$>
  MP.some (MP.alphaNumChar MP.<|> MP.char '\'' MP.<|> MP.char '-')

ingredientName :: Parser AD.IngredientName
ingredientName = AD.ingredientName . T.unwords <$>
  namePart `MP.sepEndBy1` MP.space1

effectName :: Parser AD.EffectName
effectName = AD.effectName . T.unwords <$>
  namePart `MP.sepEndBy1` MP.space1
