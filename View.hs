{-# OPTIONS_GHC -Wno-orphans #-}  -- shhhh

module View where
  
import Postlude

import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.List (sortOn)

import qualified Distn as Distn
import Extended
import State
import Slug
import Pretty
import Time
import Bounds

-- don't worry about it
instance Show (Instant -> Text) where
  show f = Text.unpack $ f (unsafePerformIO getNow)
instance Show (State -> Text) where
  show f = Text.unpack $ f (unsafePerformIO getState)

grid :: [[Text]] -> Text
grid rows =
    rows
    & fmap (\row -> row
                  & zip [0..]
                  & fmap (\(col, cell) -> padTo (columnWidths !! col) cell)
                  & mconcat)
    & Text.intercalate "\n"
    where
        columnWidths =
            [0 .. length (head rows) - 1]
            & fmap (\col -> rows
                          & fmap ((!! col) >>> Text.length)
                          & maximum)
        padTo n s = mtimes (n - Text.length s) " " <> s

history :: Slug Pool -> State -> Text
history poolSlug state =
  state `relevantMovements` poolSlug
  & fmap (\movement ->
      [ movement_text movement
      , ": "
      , pretty . fmap Money . (* movement `signWrt` poolSlug) . Distn.area . movement_distn $ movement
      , " [" <> Text.intercalate ", " (toList $ movement_tags movement) <> "]"
      ])
  & grid

currentValue :: Slug Pool -> State -> Extended Rational
currentValue poolSlug state = cumulativeDistn state poolSlug & Distn.integrate (NegInf `thru` Finite time)
  where time = unSeconds . toSecondsSinceEpoch . state_time $ state

currentValue_p :: Slug Pool -> State -> Text
currentValue_p poolSlug state = pretty . fmap Money $ currentValue poolSlug state

overview :: State -> Text
overview state =
    state_pools state
    & sortOn movementCount
    & fmap (\pool ->
        [ unSlug . pool_slug $ pool
        , ": "
        , currentValue_p (pool_slug pool) state
        , " ("
        , pretty . fmap (Rate . Money) . currentRate . pool_slug $ pool
        , ")"
        ])
    & grid

  where
    
    currentRate poolSlug = cumulativeDistn state poolSlug & Distn.evaluate (unSeconds . toSecondsSinceEpoch . state_time $ state)

    movementCount = length . relevantMovements state . pool_slug
