module Bounds (Bounds, lo, hi, mkBounds, thru) where

data Bounds n = Bounds { lo :: n, hi :: n }
  deriving (Show, Ord, Eq)

-- Smart constructor ensuring order
mkBounds :: Ord n => n -> n -> Bounds n
mkBounds a b = if a <= b then Bounds a b else Bounds b a

infixl 5 `thru`
thru :: Ord n => n -> n -> Bounds n
a `thru` b = mkBounds a b
