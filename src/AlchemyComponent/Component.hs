{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}


-- | Defines a way of constructing a data structure that stores incoming alchemy
-- information, such as the result of combining some ingredients.
--
-- The data structure is built by combining 'Component' types, each of which
-- is responsible for computing some bit of information about alchemy in Skyrim
-- based on the results of combining ingredients. For example, one component
-- may keep track of all of the effects every ingredient is known to have, and
-- another may be responsible for keeping track of the known overlap between
-- every pair of components. One does not need to know about one component to
-- be able to understand or implement the other, which makes them good
-- candidates for keeping in separate modules.
--
-- The power of this way of organizing code comes from allowing components to
-- read from other components. For instance, a component that tracks the set
-- set of fully-discovered ingredients would read from a component that tracks
-- the known effects of each ingredient. (TODO: Why this abstraction is the
-- best way to implement this)
module AlchemyComponent.Component
  ( AlchemyComponents
  , Alchemy (..)

  , Component (..)
  , Has (..)

  , OverlapValidationError (..)
  ) where


import           AlchemyData
    ( Overlap )
import qualified Control.Algebra              as Algebra
import           Control.Carrier.Error.Either
    ( Error, throwError )
import           Control.Monad.Extra
    ( whenJustM )
import           Data.Coerce
    ( coerce )
import           Data.HList
    ( HList (HCons, HEmpty) )
import qualified Data.HList                   as HList
import           Data.Proxy
    ( Proxy (Proxy) )
import qualified Data.Text                    as T


-- | Information about Skyrim's alchemy, built from individual 'Component'
-- types that track specific bits of information.
--
-- One can think of this as a record with a field for each type @c@ in @cs@.
newtype AlchemyComponents cs = AlchemyComponents (HList cs)


-- | Things you can do with an 'AlchemyComponents' data type.
class Alchemy m alchemy where
  -- | Creates an empty instance representing zero alchemy knowledge.
  initialize :: m alchemy

  -- | Updates the data structure by processing the result of combining
  -- ingredients.
  learnOverlap :: Overlap -> alchemy -> m alchemy


-- | Class for types that process the results of combining ingredients in
-- Skyrim.
--
-- @alchemy@ is usually instantiated to an @AlchemyComponents@ value with
-- all available components. 'Component' instances can require other components
-- to be available in this.
--
-- @m@ is the monad in which computations are done. 'Component' instances can
-- restrict this to allow special effects in their functions.
class Monad m => Component alchemy m c where

  -- | Creates an empty instance of the component representing zero knowledge.
  initializeComponent :: Proxy alchemy -> m c

  -- | Validates that the overlap does not contradict known information.
  --
  -- Defaults to returning no errors.
  validateOverlap
    :: Overlap
    -> alchemy
    -> c
    -> m (Maybe OverlapValidationError)
  validateOverlap _ _ _ = return Nothing

  -- | Updates the component by processing the result of combining some
  -- ingredients.
  --
  -- Defaults to returning the original data structure unchanged.
  componentLearnOverlap
    :: Overlap
    -> alchemy
    -> c
    -> m c
  componentLearnOverlap _ _ c = return c


newtype OverlapValidationError = OverlapValidationError T.Text
instance Show OverlapValidationError where
  show = T.unpack . coerce


-- | Class that can be used to get an individual component from an
-- 'AlchemyComponents' structure.
--
-- This should be imported with a qualified import and used as @Component.Has@.
class Has c alchemy where
  get :: alchemy -> c

instance HList.Has c cs => Has c (AlchemyComponents cs) where
  get (AlchemyComponents cs) = HList.get cs

instance (Functor m, Alchemy' m cs cs) => Alchemy m (AlchemyComponents cs) where
  initialize = AlchemyComponents <$> initialize_ (Proxy @(AlchemyComponents cs))
  learnOverlap overlap alchemy@(AlchemyComponents cs) =
    AlchemyComponents <$> learnOverlap_ overlap alchemy cs


-- | Helper class to recursively define 'Alchemy' instances for
-- 'AlchemyComponents'.
class Alchemy' m cs tail where

  initialize_ :: Proxy (AlchemyComponents cs) -> m (HList tail)

  learnOverlap_
    :: Overlap
    -> AlchemyComponents cs
    -> HList tail
    -> m (HList tail)

instance Monad m => Alchemy' m cs '[] where
  initialize_ _ = return HEmpty
  learnOverlap_ _ _ tail = return tail

instance ( Algebra.Has (Error OverlapValidationError) sig m
         , Component (AlchemyComponents cs) m c
         , Alchemy' m cs tail
         ) => Alchemy' m cs (c ': tail) where
  initialize_ proxy = HCons
      <$> initializeComponent proxy
      <*> initialize_ proxy

  learnOverlap_ overlap alchemy (HCons c tail) = do
    whenJustM (validateOverlap overlap alchemy c) $ \e ->
      throwError e

    c'    <- componentLearnOverlap overlap alchemy c
    tail' <- learnOverlap_ overlap alchemy tail

    return $ HCons c' tail'
