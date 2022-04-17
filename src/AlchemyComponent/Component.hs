{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
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
  , IsAlchemyInformation (..)
  , AlchemyHas
  , AlchemyHasBefore

  , Has (..)
  , HasUpdated (..)
  , HasInitialized (..)

  , OverlapValidationError (..)
  , IngredientEffectValidationError (..)
  ) where


import           AlchemyTypes
    ( EffectName, IngredientName, Overlap )
import qualified Control.Algebra              as Algebra
import           Control.Carrier.Error.Either
    ( ErrorC, throwError )
import           Control.Monad.Extra
    ( whenJustM )
import           Control.Monad.Trans
    ( lift )
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
data AlchemyComponents cs = AlchemyComponents { _components :: !(HList cs) }


-- | Things you can do with 'AlchemyComponents'.
class Alchemy m alchemy where
  -- | Creates an empty instance representing zero alchemy knowledge.
  initialize :: m alchemy

  -- | Updates the data structure by associating the effect with the ingredient.
  learnIngredientEffect
    :: IngredientName
    -> EffectName
    -> alchemy
    -> ErrorC IngredientEffectValidationError m alchemy

  -- | Updates the data structure by processing the result of combining
  -- ingredients.
  learnOverlap
    :: Overlap
    -> alchemy
    -> ErrorC OverlapValidationError m alchemy



-- | Class for types that process the results of combining ingredients in
-- Skyrim.
--
-- 'Component' instances can require other components to be available and can
-- require other components to be updated in a specific order by adding 'Has'
-- and 'HasUpdated' constraints on @alchemy@.
--
-- @m@ is the monad in which computations are done. 'Component' instances can
-- restrict this to allow special effects in their functions.
class Monad m => Component alchemy m c where

  -- | Creates an empty instance of the component representing zero knowledge.
  initializeComponent :: PartiallyInitialized alchemy -> m c

  -- | Validates that the overlap does not contradict known information.
  --
  -- Defaults to returning no errors.
  validateOverlap
    :: Overlap
    -> PartiallyValidated alchemy
    -> c
    -> m (Maybe OverlapValidationError)
  validateOverlap _ _ _ = return Nothing

  -- | Updates the component by processing the fact that an ingredient has
  -- an effect.
  --
  -- Defaults to returning the original data structure unchanged.
  componentLearnEffect
    :: IngredientName
    -> EffectName
    -> PartiallyUpdated alchemy
    -> c
    -> ErrorC IngredientEffectValidationError m c
  componentLearnEffect _ _ _ c = return c

  -- | Updates the component by processing the result of combining some
  -- ingredients.
  --
  -- Defaults to returning the original data structure unchanged.
  componentLearnOverlap
    :: Overlap
    -> PartiallyUpdated alchemy
    -> c
    -> m c
  componentLearnOverlap _ _ c = return c


newtype OverlapValidationError = OverlapValidationError T.Text
instance Show OverlapValidationError where
  show = T.unpack . coerce

newtype IngredientEffectValidationError = IngredientEffectValidationError T.Text
instance Show IngredientEffectValidationError where
  show = T.unpack . coerce


-- | The data types used in the 'Component' class.
class IsAlchemyInformation alchemy where
  type Snapshot alchemy
  data PartiallyInitialized alchemy
  data PartiallyValidated alchemy
  data PartiallyUpdated alchemy


-- | Constraint for 'Component' instance declarations that declares a dependency
-- on another component, without requiring that component to appear earlier
-- in the list of components.
type AlchemyHas c alchemy
  = ( Has c (PartiallyUpdated alchemy)
    , Has c (PartiallyValidated alchemy)
    , Has c (Snapshot alchemy) )

-- | Like 'AlchemyHas', but additionally requires that the specified component
-- appears before the current component.
--
-- This allows the component to use updated values from another component
-- when initializing or updating.
type AlchemyHasBefore c alchemy
  = ( AlchemyHas c alchemy
    , HasUpdated c (PartiallyUpdated alchemy)
    , HasInitialized c (PartiallyInitialized alchemy) )


-- | Class that allows getting the value of a specific component in an
-- alchemy data structure.
--
-- This is meant to be imported with a qualified import and referenced as
-- @Component.Has@ and @Component.get@.
class Has c alchemyType where
  get :: alchemyType -> c

-- | Class that allows getting the updated value of a specific component in
-- the 'componentLearnOverlap' function.
class HasUpdated c alchemyType where
  getUpdated :: alchemyType -> c

-- | Class that allows a component's initial value to depend on the initial
-- value of another component.
class HasInitialized c alchemyType where
  getInitialized :: alchemyType -> c


--------------------------------------------------------------------------------
-- Implementation details                                                     --
--------------------------------------------------------------------------------



data ComponentData (all :: [*]) (preceding :: [*])

instance IsAlchemyInformation (ComponentData all preceding) where
  type Snapshot (ComponentData all preceding)
    = AlchemyComponents all

  data PartiallyValidated (ComponentData all preceding)
    = PartiallyValidated
      -- TODO: Improve this name
      { _partiallyValidatedOldData :: !(HList all) }

  data PartiallyUpdated (ComponentData all preceding)
    = PartiallyUpdated
      { _oldData :: !(HList all)
      , _newData :: !(HList preceding) }

  data PartiallyInitialized (ComponentData all preceding)
    = PartiallyInitialized
      { _initialized :: !(HList preceding) }



instance HList.Has c cs
    => Has c (AlchemyComponents cs)
  where
    get = HList.get . _components

instance HList.Has c cs
    => Has c (PartiallyValidated (ComponentData cs preceding))
  where
    get = HList.get . _partiallyValidatedOldData

instance HList.Has c cs
    => Has c (PartiallyUpdated (ComponentData cs preceding))
  where
    get = HList.get . _oldData

instance HList.Has c preceding
    => HasUpdated c (PartiallyUpdated (ComponentData cs preceding))
  where
    getUpdated = HList.get . _newData

instance HList.Has c preceding
    => HasInitialized c (PartiallyInitialized (ComponentData cs preceding))
  where
    getInitialized = HList.get . _initialized


instance ( Algebra.Algebra sig m
         , AlchemyInitialize m cs cs '[]
         , AlchemyLearn m cs cs '[]
         )
    => Alchemy m (AlchemyComponents cs)
  where
    initialize
      = AlchemyComponents <$>
          initializeRecursively (Proxy @cs) HEmpty

    learnIngredientEffect ing eff alchemy
      = AlchemyComponents <$>
          learnEffectRecursively ing eff alchemy (_components alchemy) HEmpty

    learnOverlap overlap alchemy
      = AlchemyComponents <$>
          learnOverlapRecursively overlap alchemy (_components alchemy) HEmpty

class AlchemyInitialize m (all :: [*]) remaining initialized where
  initializeRecursively :: Proxy all -> HList initialized -> m (HList remaining)
class AlchemyLearn m all remaining updated where
  learnEffectRecursively
    :: Algebra.Algebra sig m
    => IngredientName
    -> EffectName
    -> AlchemyComponents all
    -> HList remaining
    -> HList updated
    -> ErrorC IngredientEffectValidationError m (HList remaining)
  learnOverlapRecursively
    :: Algebra.Algebra sig m
    => Overlap
    -> AlchemyComponents all
    -> HList remaining
    -> HList updated
    -> ErrorC OverlapValidationError m (HList remaining)


instance Monad m => AlchemyInitialize m all '[] cs where
  initializeRecursively _ _ = return HEmpty

instance ( Monad m
         , Component (ComponentData all initialized) m c
         , AlchemyInitialize m all remaining (c ': initialized) )
    => AlchemyInitialize m all (c ': remaining) initialized
  where
    initializeRecursively proxy initialized = do
      c' <- initializeComponent $
        PartiallyInitialized @all initialized
      remaining' <- initializeRecursively proxy (HCons c' initialized)
      return $ HCons c' remaining'


instance Monad m => AlchemyLearn m all '[] cs where
  learnEffectRecursively _ _ _ _ _ = return HEmpty
  learnOverlapRecursively _ _ _ _ = return HEmpty

instance ( Component (ComponentData all updated) m c
         , AlchemyLearn m all remaining (c ': updated)
         ) => AlchemyLearn m all (c ': remaining) updated where
  -- TODO: Merge these implementations

  learnEffectRecursively ing eff all (HCons c remaining) updated = do
    let partiallyUpdated = PartiallyUpdated
                           { _oldData = _components all
                           , _newData = updated }
    c' <- componentLearnEffect ing eff partiallyUpdated c
    remaining' <-
      learnEffectRecursively ing eff all remaining (HCons c' updated)

    return $ HCons c' remaining'

  learnOverlapRecursively overlap all (HCons c remaining) updated = do
    let partiallyValidated = PartiallyValidated @all @updated (_components all)
    whenJustM (lift $ validateOverlap overlap partiallyValidated c) $ \e ->
      throwError e

    let partiallyUpdated = PartiallyUpdated
                           { _oldData = _components all
                           , _newData = updated }

    c' <- lift $ componentLearnOverlap overlap partiallyUpdated c
    remaining' <-
      learnOverlapRecursively overlap all remaining (HCons c' updated)

    return $ HCons c' remaining'
