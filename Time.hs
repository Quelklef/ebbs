module Time where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Clock (NominalDiffTime)
import Data.Ratio ((%))

newtype Seconds = Seconds { unSeconds :: Rational }
  deriving (Show, Eq, Ord, Num)

-- implemented as seconds since epoch
newtype Instant = Instant Seconds
    deriving (Show, Eq, Ord)

getNow :: IO Instant
getNow = do
    time <- getPOSIXTime
    let seconds = (%) (round . (* (1e9 :: NominalDiffTime)) $ time) 1_000_000_000
    pure . fromSecondsSinceEpoch . Seconds $ seconds

-- implemented as seconds
newtype Duration = Duration Seconds
    deriving (Show)

fromSecondsSinceEpoch :: Seconds -> Instant
fromSecondsSinceEpoch = Instant

toSecondsSinceEpoch :: Instant -> Seconds
toSecondsSinceEpoch (Instant s) = s

minus :: Instant -> Instant -> Duration
Instant t0 `minus` Instant t1 = Duration (t0 - t1)
