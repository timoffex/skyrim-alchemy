-- | Defines types used everywhere in this repository to represent alchemy data.
module AlchemyTypes where

import           Data.Coerce
    ( coerce )
import           Data.Set
    ( Set )
import qualified Data.Text   as T


newtype IngredientName
  = IngredientName T.Text
  deriving ( Eq, Ord )

newtype EffectName
  = EffectName T.Text
  deriving ( Eq, Ord )


data Overlap
  = Overlap IngredientName IngredientName (Set EffectName)
  deriving ( Eq, Ord )


-- Let these names print in title case
instance Show IngredientName where
  show = T.unpack . T.toTitle . coerce
instance Show EffectName where
  show = T.unpack . T.toTitle . coerce


-- Ignore case in names
ingredientName :: T.Text -> IngredientName
ingredientName = IngredientName . T.toLower

effectName :: T.Text -> EffectName
effectName = EffectName . T.toLower
