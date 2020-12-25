{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           Alchemy              (Alchemy, EffectName (EffectName),
                                       IngredientData (IngredientData),
                                       IngredientName (IngredientName),
                                       getNonOverlappingIngredients,
                                       ingredientDataEffects,
                                       ingredientDataName,
                                       ingredientDataNonOverlaps,
                                       learnNonOverlapping, listAllEffects,
                                       listAllIngredients, runAlchemyC,
                                       snapshotData, snapshotIngredient)
import           Control.Carrier.Lift (Has, Lift, runM, sendIO)
import           Control.Monad        (forM, forever, replicateM_)
import           Data.Coerce          (coerce)
import           Data.Foldable        (forM_)
import           Data.List            (intercalate)
import           Data.List.Split      (dropBlanks, dropDelims, onSublist, split)
import qualified Data.Set             as S
import qualified Data.Text            as T
import           System.Environment   (getArgs)
import           System.IO            (hFlush, stdout)


-- | Attempts to parse a string as an 'IngredientRow'.
parseIngredientRow
  :: String -> Maybe IngredientData
parseIngredientRow s =
  let
    splitOnCommas = split (dropDelims $ dropBlanks $ onSublist ",")
    splitOnColon = split (dropDelims $ onSublist ":")
  in case splitOnColon s of
    [ingName,effs,others] ->
      let
        effNames = splitOnCommas effs
        otherNames = splitOnCommas others
      in Just $ IngredientData
           (coerce ingName)
           (S.fromList $ coerce effNames)
           (S.fromList $ coerce otherNames)
    _ -> Nothing


unparseIngredientRow
  :: IngredientData -> String
unparseIngredientRow x =
  show (ingredientDataName x) ++
  ":" ++
  intercalate "," (show <$> S.toList (ingredientDataEffects x)) ++
  ":" ++
  intercalate "," (show <$> S.toList (ingredientDataNonOverlaps x))


main :: IO ()
main = do
  [fileName] <- getArgs
  !contents  <- readFile fileName

  -- Read in ingredients
  rows <- forM (indexedLines contents) $ \(i, line) -> do
    case parseIngredientRow line of
      Nothing  -> fail $
                  "Error in ingredients file on line " ++
                  show i ++
                  ": '" ++ line ++ "'"
      Just row -> return row

  putStrLn "Parsed successfully!"

  runM $ runAlchemyC rows $ do
    sendIO $ putStrLn "Printing all effects..."
    effs <- listAllEffects
    forM_ effs $ sendIO . print


    sendIO $ putStrLn "Printing all ingredients..."
    ings <- listAllIngredients
    forM_ ings $ \ing -> do
      snapshot <- snapshotIngredient ing
      sendIO $ print snapshot

    replicateM_ 3 $ sendIO $ putStrLn ""


    sendIO $ putStrLn "Enter names of two non-overlapping ingredients..."
    -- name1 <- prompt "Name 1: " <&> trimToIngredientName
    -- name2 <- prompt "Name 2: " <&> trimToIngredientName
    let name1 = coerce "Purple Mountain Flower"
        name2 = coerce "Blue Mountain Flower"
    learnNonOverlapping name1 name2


    forever interactNonOverlapping


    sendIO $ putStrLn "Saving back to output.txt..."
    finalData <- snapshotData
    sendIO $ writeFile "output.txt" $
      unlines (unparseIngredientRow <$> finalData)



interactNonOverlapping
  :: ( Has Alchemy sig m
     , Has (Lift IO) sig m
     )
  => m ()
interactNonOverlapping = do
  name <- prompt "Enter an ingredient name: " <&> trimToIngredientName

  others <- getNonOverlappingIngredients name
  sendIO $ putStrLn $
    "Non-overlapping ingredients for " ++ coerce name ++ ":"
  forM_ others $ sendIO . print


prompt :: ( Has (Lift IO) sig m ) => String -> m String
prompt s = sendIO $ do
  putStr s
  hFlush stdout
  getLine


trimToIngredientName :: String -> IngredientName
trimToIngredientName = IngredientName . T.unpack . T.strip . T.pack


indexedLines :: String -> [(Int, String)]
indexedLines = zip [0..] . lines


(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
