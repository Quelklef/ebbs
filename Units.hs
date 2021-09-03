{-# LANGUAGE NoMonomorphismRestriction   #-}
{-# OPTIONS_GHC -w                       #-}

module Units where

import Postlude

(second : seconds : _) = repeat $ (1 / 1 :: Fractional n => n)
(minute : minutes : _) = repeat $ 60 * second
(hour   : hours   : _) = repeat $ 60 * minute

(year   : years   : _) = repeat $ 31_557_600 * seconds
(month  : months  : _) = repeat $ year / 12
(week   : weeks   : _) = repeat $ year / 52
(day    : days    : _) = repeat $ week / 7
