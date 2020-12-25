{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Alchemy
  ( IngredientName (IngredientName)
  , EffectName (EffectName)
  , Effect

  , Ingredient
  , ingredientName
  , ingredientEffects
  , snapshotIngredient

  , IngredientData (IngredientData)
  , ingredientDataName
  , ingredientDataEffects
  , ingredientDataNonOverlaps

  , Alchemy
  , snapshotData
  , listAllEffects
  , listAllIngredients
  , getEffectsFor
  , getNonOverlappingIngredients
  , learnNonOverlapping

  , AlchemyC
  , runAlchemyC
  ) where

import           Control.Algebra           ((:+:) (..), Algebra (..), Has, send)
import           Control.Applicative       (Alternative (empty))
import           Control.Effect.Lift       (Lift, sendIO)
import           Control.Effect.Sum        (Member)
import           Control.Lens              (at, folded, hasn't, ix, makeFields,
                                            makePrisms, to, toListOf, view,
                                            (%=), (&), (^.), (^..), (^?!))
import           Control.Monad             (forM_, when)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.ST          (runST)
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.State (StateT (StateT), evalStateT, get,
                                            gets)
import           Data.Coerce               (coerce)
import           Data.Foldable             (Foldable (fold))
import           Data.Functor              (($>), (<&>))
import           Data.Kind                 (Type)
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import           Data.STRef                (modifySTRef, newSTRef, readSTRef)


--------------------------------------------------------------------------------
-- Ingredients & alchemical effects
--------------------------------------------------------------------------------


newtype IngredientName
  = IngredientName String
  deriving ( Eq, Ord )

newtype EffectName
  = EffectName String
  deriving ( Eq, Ord )


-- Let these names print like normal strings
instance Show IngredientName where
  show = coerce
instance Show EffectName where
  show = coerce


data Effect m
  = RealEffect EffectName
  | PhantomEffect (Ingredient m) Int

-- | An identifier for effects.
--
-- Phantom effects don't have names. Instead, they are identified by
-- an integer and an ingredient name. Every ingredient has up to
-- 'minEffects' phantom effects.
data EffectId
  = RealEffectId EffectName
  | PhantomEffectId IngredientName Int
  deriving ( Show, Eq, Ord )


data Ingredient m
  = Ingredient
    { _ingredientName    :: IngredientName
    , _ingredientEffects :: m [Effect m] }
$( makePrisms ''EffectId )
$( makeFields ''Ingredient )

data IngredientData
  = IngredientData
    { _ingredientDataName        :: IngredientName
    , _ingredientDataEffects     :: S.Set EffectName
    , _ingredientDataNonOverlaps :: S.Set IngredientName }
  deriving ( Eq, Ord )
$( makeFields ''IngredientData )


ingredientDataName :: IngredientData -> IngredientName
ingredientDataName = _ingredientDataName

ingredientDataEffects :: IngredientData -> S.Set EffectName
ingredientDataEffects = _ingredientDataEffects

ingredientDataNonOverlaps :: IngredientData -> S.Set IngredientName
ingredientDataNonOverlaps = _ingredientDataNonOverlaps

ingredientName :: Ingredient m -> IngredientName
ingredientName = view name

ingredientEffects :: Ingredient m -> m [Effect m]
ingredientEffects = view effects

getEffectId :: Effect m -> EffectId
getEffectId = \case
  RealEffect n -> RealEffectId n
  PhantomEffect ing i -> PhantomEffectId (ing^.name) i


instance Show (Effect m) where
  show (RealEffect n) = coerce n
  show (PhantomEffect ing i) =
    "phantom "
    ++ show i
    ++ " ("
    ++ coerce (ing^.name)
    ++ ")"

instance Show (Ingredient m) where
  show = coerce . view name

instance Show IngredientData where
  show ingDat =
    coerce (ingDat^.name) ++
    " " ++
    show @[String] (coerce $ S.toList (ingDat^.effects))


--------------------------------------------------------------------------------
-- The (monadic) "effect" for using alchemy knowledge
--------------------------------------------------------------------------------


-- | An effect for using alchemy knowledge.
data Alchemy (m :: Type -> Type) a where
  GetEffectsFor
    :: Has Alchemy sig m
    => IngredientName
    -> Alchemy m [Effect m]
  ListAllEffects
    :: Has Alchemy sig m
    => Alchemy m [Effect m]
  ListAllIngredients
    :: Has Alchemy sig m
    => Alchemy m [Ingredient m]
  GetNonOverlappingIngredients
    :: Has Alchemy sig m
    => IngredientName
    -> Alchemy m [Ingredient m]

  -- TODO:
  {-
  LearnEffect
    :: Has Alchemy sig m
    => IngredientName
    -> EffectName
    -> Alchemy m ()
  -}
  LearnNonOverlapping
    :: Has Alchemy sig m
    => IngredientName
    -> IngredientName
    -> Alchemy m ()

  SnapshotData
    :: Has Alchemy sig m
    => Alchemy m [IngredientData]

getEffectsFor
  :: Has Alchemy sig m
  => IngredientName -> m [Effect m]
getEffectsFor = send . GetEffectsFor


listAllEffects
  :: Has Alchemy sig m
  => m [Effect m]
listAllEffects = send ListAllEffects


listAllIngredients
  :: Has Alchemy sig m
  => m [Ingredient m]
listAllIngredients = send ListAllIngredients


learnNonOverlapping
  :: Has Alchemy sig m
  => IngredientName
  -> IngredientName
  -> m ()
learnNonOverlapping n1 n2 = send $ LearnNonOverlapping n1 n2


snapshotData
  :: Has Alchemy sig m
  => m [IngredientData]
snapshotData = send SnapshotData


snapshotIngredient
  :: Has Alchemy sig m
  => Ingredient m -> m IngredientData
snapshotIngredient ing = do
  effs <- ing^.effects
  others <- getNonOverlappingIngredients (ing^.name)

  let effNames = S.fromList $ do
        eff <- effs
        case eff of
          RealEffect n -> return n
          _            -> empty

      otherNames = S.fromList $ view name <$> others

  return $ IngredientData (ing^.name) effNames otherNames



getNonOverlappingIngredients
  :: Has Alchemy sig m
  => IngredientName
  -> m [Ingredient m]
getNonOverlappingIngredients =
  send . GetNonOverlappingIngredients

--------------------------------------------------------------------------------
-- The implementation of 'Alchemy'
--------------------------------------------------------------------------------

newtype AlchemyC m a
  = AlchemyC
    { unAlchemyC :: StateT AlchemyData m a }
  deriving ( Functor, Applicative, Monad, MonadIO )

instance MonadTrans AlchemyC where
  lift = AlchemyC . lift


data AlchemyData
  = AlchemyData
    { _alchemyDataPositives            :: M.Map EffectId (S.Set IngredientName)
    , _alchemyDataNegatives            :: M.Map EffectId (S.Set IngredientName)
    , _alchemyDataAllIngredients       :: M.Map IngredientName (S.Set EffectId)
    , _alchemyDataCompletedIngredients :: S.Set IngredientName }
$( makeFields ''AlchemyData )


runAlchemyC
  :: Monad m
  => [IngredientData]
  -> AlchemyC m a
  -> m a
runAlchemyC ingredientList (AlchemyC alch) =
  evalStateT alch $ mkAlchemyData ingredientList


-- | The minimum number of effects that every ingredient has.
--
-- When fewer than this many effects are known for an ingredient,
-- phantom effects are created.
minEffects :: Integral a => a
minEffects = 4


-- | Creates an 'Effect' from an 'EffectId'.
--
-- The fact that this can be done in a pure way is an implementation
-- detail.
mkEffect
  :: Has Alchemy sig m
  => EffectId -> Effect m
mkEffect = \case
  RealEffectId n -> RealEffect n
  PhantomEffectId ingId i ->
    PhantomEffect (mkIngredient ingId) i


-- | Creates an 'Ingredient' from an 'IngredientName'.
--
-- The fact that this can be done in a pure way is an implementation
-- detail.
mkIngredient
  :: Has Alchemy sig m
  => IngredientName -> Ingredient m
mkIngredient ingId = Ingredient ingId (getEffectsFor ingId)


instance ( Algebra sig m
         , Member (Lift IO) sig
         ) => Algebra (Alchemy :+: sig) (AlchemyC m) where
  alg hdl sig ctx = case sig of


    L ListAllEffects -> AlchemyC $ do
      alchemyData <- get
      let
        allEffectIds = M.keys (alchemyData^.positives)
        allEffects = mkEffect <$> allEffectIds
      return (ctx $> allEffects)

    L ListAllIngredients -> AlchemyC $ do
      alchemyData <- get
      let
        ingredientIds = M.keys $ alchemyData^.allIngredients
        ingredients = mkIngredient <$> ingredientIds
      return (ctx $> ingredients)


    L (GetEffectsFor ingName) -> AlchemyC $ do
      alchemyData <- get
      let
        effectIds = S.toList $
          fold $ M.lookup ingName (alchemyData^.allIngredients)
        ingEffects = mkEffect <$> effectIds
      return (ctx $> ingEffects)

    L (GetNonOverlappingIngredients ingName) -> AlchemyC $ do
      alchemyData <- get
      let
        ingEffectIds = S.toList $
          fold $ M.lookup ingName (alchemyData^.allIngredients)
        ingredientNames = foldl1 S.intersection
          (ingEffectIds <&> \effId ->
              fold $ M.lookup effId (alchemyData^.negatives))
        ingredients =
          if null ingEffectIds
          then []
          else mkIngredient <$> S.toList ingredientNames
      return (ctx $> ingredients)

    L (LearnNonOverlapping i1 i2) -> AlchemyC $ do
      let getEffsOf i = gets $ toListOf (allIngredients.ix i.to S.toList.traverse)
      effs1 <- getEffsOf i1
      effs2 <- getEffsOf i2

      forM_ effs1 $ \eff1Name ->
        negatives %= multiMapInsert eff1Name i2
      forM_ effs2 $ \eff2Name ->
        negatives %= multiMapInsert eff2Name i1

      return ctx


    L SnapshotData -> do
      ingredients <- listAllIngredients
      ingredientData <- mapM snapshotIngredient ingredients
      return (ctx $> ingredientData)


    R other -> AlchemyC $ alg (unAlchemyC . hdl) (R other) ctx





mkAlchemyData :: [IngredientData] -> AlchemyData
mkAlchemyData ingredientList = runST $ do
  -- Deduplicate!
  let ingredientData = S.fromList ingredientList

  positivesRef <- newSTRef M.empty
  negativesRef <- newSTRef M.empty
  allIngredientsRef <- newSTRef M.empty
  completedIngredientsRef <- newSTRef S.empty

  forM_ ingredientData $ \ing -> do

    -- Add to completed ingredient set if ing is completed
    when (S.size (ing^.effects) >= minEffects) $
      modifySTRef completedIngredientsRef $
        S.insert (ing^.name)

    let
      realEffs = RealEffectId <$> S.toList (ing^.effects)
      numEffs = S.size (ing^.effects)
      phantomEffs =
        [ PhantomEffectId (ing^.name) i
        | i <- [0..minEffects - numEffs - 1] ]

      allEffs = S.fromList (realEffs ++ phantomEffs)

    -- Populate ingredient->effect map
    modifySTRef allIngredientsRef $
      M.insertWith (<>) (ing^.name) allEffs

    -- Populate effect->ingredient map
    forM_ allEffs $ \eff -> do
      modifySTRef positivesRef $
        multiMapInsert eff (ing^.name)

  -- Populate the negative effect->ingredient map
  --
  -- Initially, we only know for sure that a complete ingredient that
  -- doesn't have a non-phantom effect definitely does not have that
  -- effect (we can't say the same for phantom effects).
  completedIngredientsSet <- readSTRef completedIngredientsRef
  positivesList <- M.assocs <$> readSTRef positivesRef
  forM_ positivesList $ \(effId, ings) ->
    when (effId & hasn't _PhantomEffectId) $
    modifySTRef negativesRef $
        M.insert effId (completedIngredientsSet S.\\ ings)

  -- Populate the negative effect->ingredient map with the provided
  -- non-overlap data.
  ingredientsToEffects <- readSTRef allIngredientsRef
  forM_ ingredientData $ \ingData -> do
    let
      thisName = ingData^.name
      thisEffs = ingredientsToEffects^..ix thisName.folded
    forM_ thisEffs $ \effId -> do
      forM_ (ingData^.nonOverlaps) $ \otherName -> do
        modifySTRef negativesRef $
          multiMapInsert effId otherName

  AlchemyData <$>
    readSTRef positivesRef <*>
    readSTRef negativesRef <*>
    readSTRef allIngredientsRef <*>
    readSTRef completedIngredientsRef





--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

multiMapInsert
  :: ( Ord k, Ord a )
  => k -> a -> M.Map k (S.Set a) -> M.Map k (S.Set a)
multiMapInsert k a = M.insertWith (<>) k (S.singleton a)
