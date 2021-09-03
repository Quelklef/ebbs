module State where

import Postlude

import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Time.Format as Time
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.POSIX as Time

import qualified Units as Units
import Extended
import Distn
import Slug
import Time

data State = State
    { state_pools :: [Pool]
    , state_movements :: [Movement]
    , state_time :: Instant
    } deriving (Show)

data Pool = Pool
    { pool_slug :: Slug Pool
    , pool_text :: Text
    } deriving (Show, Eq)

data Movement = Movement
    { movement_distn :: Distn
    , movement_source :: Slug Pool
    , movement_target :: Slug Pool
    , movement_post :: Instant  -- real-world post time
    , movement_text :: Text
    , movement_tags :: Set Text
    } deriving (Show)

relevantMovements :: State -> Slug Pool -> [Movement]
relevantMovements state pool =
  state_movements state
  & filter (\movement -> pool `elem` [movement_source movement, movement_target movement])

-- calculate the cumulative movement for some pool
cumulativeDistn :: State -> Slug Pool -> Distn
cumulativeDistn state pool =
  state_movements state
  & fmap (\movement -> case movement `signWrt` pool of
    0 -> Nothing
    s -> Just $ Distn.scale s (movement_distn movement))
  & catMaybes
  & toList
  & Distn.add

signWrt :: Num n => Movement -> Slug Pool -> n
signWrt movement pool =
  let source = movement_source movement
      target = movement_target movement
  in if
    | source == pool && target == pool -> 0
    | source == pool -> -1
    | target == pool -> 1
    | otherwise -> 0

-- ⣿⣿⣿⠟⠛⠛⠻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡟⢋⣩⣉⢻⣿⣿
-- ⣿⣿⣿⠀⣿⣶⣕⣈⠹⠿⠿⠿⠿⠟⠛⣛⢋⣰⠣⣿⣿⠀⣿⣿
-- ⣿⣿⣿⡀⣿⣿⣿⣧⢻⣿⣶⣷⣿⣿⣿⣿⣿⣿⠿⠶⡝⠀⣿⣿
-- ⣿⣿⣿⣷⠘⣿⣿⣿⢏⣿⣿⣋⣀⣈⣻⣿⣿⣷⣤⣤⣿⡐⢿⣿
-- ⣿⣿⣿⣿⣆⢩⣝⣫⣾⣿⣿⣿⣿⡟⠿⠿⠦⠀⠸⠿⣻⣿⡄⢻
-- ⣿⣿⣿⣿⣿⡄⢻⣿⣿⣿⣿⣿⣿⣿⣿⣶⣶⣾⣿⣿⣿⣿⠇⣼
-- ⣿⣿⣿⣿⣿⣿⡄⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡟⣰⣿
-- ⣿⣿⣿⣿⣿⣿⠇⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⢀⣿⣿
-- ⣿⣿⣿⣿⣿⠏⢰⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⢸⣿⣿
-- ⣿⣿⣿⣿⠟⣰⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⣿⣿
-- ⣿⣿⣿⠋⣴⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡄⣿⣿
-- ⣿⣿⠋⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⢸⣿

getState :: IO State
getState = do
  now <- getNow

  let
    pconj  = pool "conj"      "Where expenditures go to and payments come from"
    pspare = pool "spare"     "Unbudgeted cash"
    pMS    = pool "m stanley" "Money invested at Morgan Stanley"
    pfood  = pool "food"      "Yummy"
    prent  = pool "rent"      "Got 2 live somewhere"

  pure $ State
    { state_time = now
    , state_pools = [pconj, pspare, pMS, pfood, prent]
    , state_movements =
      [ {- redacted -}

      , {- redacted -}

      , move "Food budget" ""
             (time "2021-04-26 19:09:51 -07:00 (PDT)")
             pspare pfood (Distn.henceforth $ rat "300" `per` Units.month)

      , move "Rent budget" ""
             (time "2021-04-27 09:37:15 -07:00 (PDT)")
             pspare prent (Distn.henceforth $ rat "1000" `per` Units.month)

      , move "Dinner" ""
             (time "2021-04-26 20:22:33 -07:00 (PDT)")
             pfood pconj (rat "32.62" & Distn.instantly)

      , move "Safeway: kombucha" ""
             (time "2021-04-27 22:19:00 -07:00 (PDT)")
             pfood pconj (rat "7.58" & Distn.instantly)

      , move "Safeway: soap + sharpies" ""
             (time "2021-04-27 22:19:00 -07:00 (PDT)")
             pspare pconj ((rat "23.25" - rat "7.58") & Distn.instantly)

      , move "Safeway: snacks" ""
             (time "2021-04-28 00:00:00 -07:00 (PDT)")
             pfood pconj (rat "16.16" & Distn.instantly)

      , move "7-11: food" ""
             (time "2021-04-29 00:00:00 -07:00 (PDT)")
             pfood pconj (rat "27.35" & Distn.instantly)

      , move "Lyft" "transportation"
             (time "2021-04-29 00:00:00 -07:00 (PDT)")
             pspare pconj (rat "31.10" & Distn.instantly)

      , move "Lyft" "transportation"
             (time "2021-04-30 00:00:00 -07:00 (PDT)")
             pspare pconj (rat "37.19" & Distn.instantly)

      , move "Trader Joe's: pizza stuff" ""
             (time "2021-05-01 00:00:00 -07:00 (PDT)")
             pfood pconj (rat "20.22" & Distn.instantly)

      -- doorknobs are fuckin' exp*nsive holy moly
      , move "Ace Hardware: new doorknob + duc[tk] tape" ""
             (time "2021-05-01 00:00:00 -07:00 (PDT)")
             pspare pconj (rat "47.78" & Distn.instantly)

      , move "7-11: 7-up for alcy" ""
             (time "2021-05-02 20:03:00 -07:00 (PDT)")
             pfood pconj (rat "2.69" & Distn.instantly)

      , move "minecraft server? lol" ""
             (time "2021-05-04 00:00:00 -07:00 (PDT)")
             pspare pconj (rat "19.52" & Distn.instantly)

      , move "paycheck" ""
             (time "2021-05-04 00:00:00 -07:00 (PDT)")
             pconj pspare (rat "9,120.00" & Distn.instantly)

      , move "hangover food" ""
             (time "2021-05-03 18:00:00 -07:00 (PDT)")
             pfood pconj (rat "20.85" & Distn.instantly)

      , move "health insurance" ""
             (time "2021-05-04 00:00:00 -07:00 (PDT)")
             pspare pconj (rat "206.39" & Distn.instantly)

      , move "food" ""
             (time "2021-05-05 13:15:00 -07:00 (PDT)")
             pfood pconj (rat "79.84" & Distn.instantly)

      , move "dashpass" "sub"
             (time "2021-05-08 22:03:00 -07:00 (PDT)")
             pspare pconj (rat "9.99" & Distn.instantly)

      , move "milk" ""
             (time "2021-05-10 22:02:00 -07:00 (PDT)")
             pfood pconj (rat "1.49" & Distn.instantly)

      , move "trimet card + paper towels + dish soap" ""
             (time "2021-05-10 22:02:00 -07:00 (PDT)")
             pspare pconj ((rat "5.49" + rat "3.99" + rat "23.00") & Distn.instantly)

      , move "mcdonald" ""
             (time "2021-05-11 15:12:00 -07:00 (PDT)")
             pfood pconj (rat "16.00" & Distn.instantly)

      , move "dinner" ""
             (time "2021-05-13 19:26:00 -07:00 (PDT)")
             pfood pconj (rat "24.00" & Distn.instantly)

      , move "ordered food" ""
             (time "2021-05-14 10:52:00 -07:00 (PDT)")
             pfood pconj (rat "68.34" & Distn.instantly)

      , move "debt forgiveness" ""
             (time "2021-05-14 16:09:00 -07:00 (PDT)")
             pspare pfood (rat "200.00" & Distn.instantly)

      , move "groccs" ""
             (time "2021-05-14 16:55:00 -07:00 (PDT)")
             pfood pconj (Distn.amortize $
              [ rat "5.49" `Distn.over` (4 * meals) -- beans
              , rat "1.99" `Distn.over` (3 * meals) -- rice
              , rat "1.25" `Distn.over` (0 * meals) -- ???
              , rat "3.99" `Distn.over` (2 * meals) -- peanut butter
              , rat "2.29" `Distn.over` (4 * meals) -- eggs
              , rat "1.49" `Distn.over` (2 * meals) -- milk
              , rat "1.99" `Distn.over` (3 * meals) -- yogurt
              , rat "2.99" `Distn.over` (2 * meals) -- butter
              , rat "8.60" `Distn.over` (3 * meals) -- beef
              , rat "2.99" `Distn.over` (0 * meals) -- leaves
              , rat "2.50" `Distn.over` (1 * meals) -- mushrooms
              , rat "1.49" `Distn.over` (2 * meals) -- carrots
              , rat "1.29" `Distn.over` (0 * meals) -- green onions
              , rat "1.49" `Distn.over` (0 * meals) -- lemon
              , rat "2.02" `Distn.over` (1 * meals) -- bananas
              , rat "0.99" `Distn.over` (1 * meals) -- onions
              , rat "3.11" `Distn.over` (1 * meals) -- oranges
              , rat "7.18" `Distn.over` (2 * meals) -- grapes
              , rat "1.99" `Distn.over` (1 * meals) -- apples
              , rat "2.50" `Distn.over` (0 * meals) -- kombucha
              , rat "0.10" `Distn.over` (0 * meals) -- ???
              , rat "1.38" `Distn.over` (1 * meals) -- avocados
              , rat "0.96" `Distn.over` (0 * meals) -- apples again wtf
              , rat "2.99" `Distn.over` (1 * meals) -- cheese
              ])

      , move "turbo tax" ""
             (time "2021-05-15 22:30:00 -07:00 (PDT)")
             pspare pconj (rat "170.00" & Distn.instantly)

      , move "groccs" ""
             (time "2021-05-17 19:27:00 -07:00 (PDT)")
             pfood pconj (Distn.amortize $
             [ rat "1.29"  `Distn.over` (0 * meals) -- mustard
             , rat "2.99"  `Distn.over` (0 * meals) -- pop tarts
             , rat "1.99"  `Distn.over` (0 * meals) -- evaporated milk
             , rat "3.99"  `Distn.over` (0 * meals) -- tarragon
             , rat "7.49"  `Distn.over` (0 * meals) -- mustard
             , rat "1.00"  `Distn.over` (2 * meals) -- ramen
             , rat "1.34"  `Distn.over` (2 * meals) -- shell pasta
             , rat "2.50"  `Distn.over` (0 * meals) -- honey mustard dijon
             , rat "1.99"  `Distn.over` (2 * meals) -- hot dog buns
             , rat "5.00"  `Distn.over` (1 * meals) -- cookies
             , rat "6.00"  `Distn.over` (3 * meals) -- sausages
             , rat "1.99"  `Distn.over` (0 * meals) -- bay leaves
             , rat "1.49"  `Distn.over` (0 * meals) -- onion powder
             , rat "1.99"  `Distn.over` (0 * meals) -- garlic powder
             , rat "11.98" `Distn.over` (1 * meals) -- pepperjack cheese
             , rat "5.99"  `Distn.over` (1 * meals) -- hummus
             , rat "7.99"  `Distn.over` (1 * meals) -- more cheese
             ])

      , move "groccs (non-food)" ""
             (time "2021-05-17 19:27:00 -07:00 (PDT)")
             pspare pconj (rat "7.99" & Distn.instantly)

      , move "groccs" ""
             (time "2021-05-18 17:02:00 -07:00 (PDT)")
             pfood pconj (Distn.amortize $
             [ rat "4.98" `Distn.over` (1 * meals) -- pineapple bits
             , rat "4.99" `Distn.over` (1 * meals) -- olive oil
             , rat "2.99" `Distn.over` (0 * meals) -- spinach
             , rat "2.04" `Distn.over` (1 * meals) -- broccoli
             ])

      , move "groccs (non-food)" ""
             (time "2021-05-18 17:02:00 -07:00 (PDT)")
             pspare pconj (rat "10.49" & Distn.instantly)

      , move "google photos subscription" ""
             (time "2021-05-20 14:35:00 -07:00 (PDT)")
             pspare pconj (rat "1.99" & Distn.instantly)

      , move "rent" ""
             (time "2021-05-20 15:58:00 -07:00 (PDT)")
             pspare pconj (rat "530.00" & Distn.instantly)

      , move "roam research sub" ""
             (time "2021-05-22 19:42:00 -07:00 (PDT)")
             pspare pconj (rat "15.00" & Distn.instantly)

      , move "clothing" ""
             (time "2021-05-23 00:05:00 -07:00 (PDT)")
             pspare pconj (rat "333.00" & Distn.instantly)

      , move "digitalocean payments" ""
             (time "2021-05-24 01:59:00 -07:00 (PDT)")
             pspare pconj (rat "101.00" & Distn.instantly)
      ]
    }

  where
    pool :: Text -> Text -> Pool
    pool slug text = Pool (Slug slug :: Slug Pool) text

    move :: Text -> Text -> Instant -> Pool -> Pool -> Distn -> Movement
    move text tags time source target distn = Movement
      { movement_distn = Distn.shift (unSeconds seconds) distn
      , movement_source = pool_slug source
      , movement_target = pool_slug target
      , movement_post = fromSecondsSinceEpoch seconds
      , movement_text = text
      , movement_tags = tags & Text.split (== ' ') & Set.fromList
      }
      where seconds = toSecondsSinceEpoch time

    -- parse a rational (with haste)
    rat :: Text -> Rational
    rat string =
      let
        clean = Text.filter (`elem` (".+-1234567890" :: [Char])) string
        (a:b:_) = fmap (read . Text.unpack) . Text.split (== '.') . (<> ".0") $ clean
      in (a % 1) + (b % ((10 ^) . length . show $ b))

    -- parse a time (unsafely)
    time = Text.takeWhile (/= '(')
       >>> Text.dropEnd 1
       >>> Text.unpack
       >>> Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z"
       >>> unsafePerformIO
       >>> Time.utcTimeToPOSIXSeconds
       >>> Time.nominalDiffTimeToSeconds
       >>> floor
       >>> (% 1)
       >>> Seconds
       >>> Instant

    meals :: Rational
    meals = Units.day / 3

    per = (/)
