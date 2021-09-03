{-# LANGUAGE FlexibleInstances #-}

module Pretty where

import Postlude

import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.List.Extra (minimumOn)

import qualified Units

class Pretty p where
  pretty :: p -> Text

newtype Money = Money { unMoney :: Rational }
  deriving (Show, Eq, Ord, Num, Fractional)

instance Pretty Money where
  pretty (Money amt) =
    let
      _100s = amt & abs & (* 100) & floor @_ @Int & (`mod` 10) & show & Text.pack
      _10s  = amt & abs & (* 10 ) & floor @_ @Int & (`mod` 10) & show & Text.pack
      comma'd = amt
              & abs
              & floor @_ @Integer
              & show
              & Text.pack
              & Text.reverse
              & Text.chunksOf 3
              & Text.intercalate ","
              & Text.reverse
      sign = if amt < 0 then "-" else "+"
    in mconcat [sign, "$", comma'd, ".", _10s, _100s]

-- Represents a rate of something *per second*
newtype Rate n = Rate { unRate :: n }
  deriving (Show, Eq)

instance (Fractional n, Ord n, Pretty n) => Pretty (Rate n) where
  -- Tries to intelligently choose a "nice" timescale on which to display the rate
  pretty (Rate rate) =
    if rate == 0 then pretty rate <> "/*" else

    let
      ideal = 50  -- the kind of size of number we're looking for
      units =
        [ (Units.hour  , "h")
        , (Units.day   , "d")
        , (Units.week  , "w")
        , (Units.month , "m")
        , (Units.year  , "y")
        ]

      unit = units
           & minimumOn (\(k, _) -> abs $ ideal - (abs $ k * rate))

    in case unit of
      (k, name) -> pretty (k * rate) <> "/" <> name
