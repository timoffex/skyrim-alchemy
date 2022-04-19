{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Implements 'AlchemyInteraction' using IO.
module AlchemyInteractionIO
  ( runAlchemyInteractionIO,
    AlchemyInteractionIO,
  )
where

import AlchemyInteraction
  ( AlchemyInteraction (..),
    IngredientForEffects (..),
  )
import Control.Algebra
  ( Algebra (..),
    Has,
    (:+:) (..),
  )
import Control.Effect.Lift
  ( Lift,
    sendIO,
  )
import Control.Monad.Trans.Class
  ( MonadTrans (..),
  )
import Data.List
  ( intercalate,
  )
import qualified Data.Set as S
import System.Exit
  ( exitSuccess,
  )

newtype AlchemyInteractionIO m a = AlchemyInteractionIO {unwrap :: m a}
  deriving (Functor, Applicative, Monad)

runAlchemyInteractionIO ::
  Has (Lift IO) sig m =>
  AlchemyInteractionIO m a ->
  m a
runAlchemyInteractionIO = unwrap

instance MonadTrans AlchemyInteractionIO where
  lift = AlchemyInteractionIO

instance
  ( Has (Lift IO) sig m
  ) =>
  Algebra
    (AlchemyInteraction :+: sig)
    (AlchemyInteractionIO m)
  where
  alg hdl sig ctx = AlchemyInteractionIO $ case sig of
    L Exit -> ctx <$ sendIO exitSuccess
    L (PrintError err) ->
      ctx <$ sendIO (putStrLn $ "Error: " ++ err)
    L (PrintEffects effs) ->
      ctx <$ sendIO (printOnSeparateLines effs)
    L (PrintIngredients ings) ->
      ctx <$ sendIO (printOnSeparateLines ings)
    L (PrintOrderedIngredientsForEffects ings) ->
      ctx <$ sendIO (printOnSeparateLines $ showIngForEff <$> ings)
    R other -> alg (unwrap . hdl) other ctx

showIngForEff :: IngredientForEffects -> String
showIngForEff (IngredientForEffects ing effs) =
  show ing ++ " for " ++ intercalate ", " (show <$> S.toList effs)

printOnSeparateLines ::
  ( Has (Lift IO) sig m,
    Foldable t,
    Show a
  ) =>
  t a ->
  m ()
printOnSeparateLines = mapM_ (sendIO . print)
