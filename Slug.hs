module Slug (Slug(..)) where

import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.String (IsString(..))

-- Identifier type
-- Intended to hold human-readable content
-- Has phantom parameter so that 'Slug A' and 'Slug B' don't unify
newtype Slug for = Slug { unSlug :: Text }
    deriving (Show, Ord, Eq)

instance IsString (Slug for) where
  fromString = Slug . Text.pack
