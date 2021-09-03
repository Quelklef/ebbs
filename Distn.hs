module Distn
  ( Distn

  , impulse
  , instantly
  , constantly
  , step
  , henceforth
  , restrict
  , shift
  , scale
  , add
  , over

  , evaluate
  , integrate
  , area
  , amortize
  ) where

import Postlude

import Extended
import Bounds

{- nb This module almost certainly has an elegant generalization
      hiding under its skin, but I was unable to tease out exactly
      what that generalization is... it gets difficult because the
      typical theory of distributions has distribution domain,
      codomain, and integral all be the same (R, Q, etc), whereas
      ideally here we would be able to nicely express in the type
      system that the domain is Seconds, codomain is Units/Second,
      and integral is Units.
-}

data Distn
  -- 0 everywhere but infinity at 0; area of 1
  = Impulse
  -- 1 everywhere
  | ConstOne
  -- restrict the domain of a distribution
  | Restricted (Bounds (Extended Rational)) Distn
  -- shift a distribution along the x axis
  | Shifted Rational Distn
  -- scale a distribution along the y axis
  | Scaled Rational Distn
  -- add many distributions
  | Add [Distn]
  deriving (Show)

-- exported distribution constructors

impulse :: Distn
impulse = Impulse

instantly :: Rational -> Distn
instantly n = Scaled n Impulse

constantly :: Rational -> Distn
constantly 1 = ConstOne
constantly n = Scaled n ConstOne

-- heaviside
step :: Distn
step = Restricted (0 `thru` PosInf) ConstOne

henceforth :: Rational -> Distn
henceforth n = Scaled n step

restrict :: Bounds (Extended Rational) -> Distn -> Distn
restrict = Restricted

shift :: Rational -> Distn -> Distn
shift = Shifted

scale :: Rational -> Distn -> Distn
scale = Scaled

add :: [Distn] -> Distn
add = Add

over :: Rational -> Rational -> Distn
amt `over` duration
  | duration == 0 = instantly amt
  | otherwise = Distn.restrict (0 `thru` Finite duration) (Distn.constantly $ amt / duration)

-- distribution operations

evaluate :: Rational -> Distn -> Extended Rational
evaluate x = \case
  Impulse -> if x == 0 then PosInf else 0
  ConstOne -> 1
  Restricted bounds d -> if Finite x < lo bounds || Finite x > hi bounds then 0 else evaluate x d
  Shifted dx d -> evaluate (x - dx) d
  Scaled sy d -> Finite sy * evaluate x d
  Add ds -> ds & fmap (\d -> evaluate x d) & sum

integrate :: Bounds (Extended Rational) -> Distn -> Extended Rational
integrate bounds = \case
  Impulse -> if lo bounds <= Finite 0 && Finite 0 <= hi bounds then 1 else 0
  ConstOne -> hi bounds - lo bounds
  Restricted b d -> integrate (max (lo bounds) (lo b) `thru` min (hi bounds) (hi b)) d
  Shifted dx d -> integrate (lo bounds - Finite dx `thru` hi bounds - Finite dx) d
  Scaled sy d -> Finite sy * integrate bounds d
  Add ds -> ds & fmap (integrate bounds) & sum

-- "contiguous domain"
-- Find the smallest interval [lo, hi] which is a superset of the distribution's domain
contDom :: Distn -> Bounds (Extended Rational)
contDom = \case
  Impulse -> mkBounds 0 0
  ConstOne -> mkBounds NegInf PosInf
  Restricted b' d -> let b = contDom d in mkBounds (max (lo b) (lo b')) (min (hi b) (hi b'))
  Shifted dx d -> let b = contDom d in mkBounds (lo b + Finite dx) (hi b + Finite dx)
  Scaled _ d -> contDom d
  Add ds -> let bs = fmap contDom ds in mkBounds (minimum . fmap lo $ bs) (maximum . fmap hi $ bs)

area :: Distn -> Extended Rational
area = integrate $ NegInf `thru` PosInf

amortize :: [Distn] -> Distn
amortize ds = unFinite sumY `over` unFinite sumX
  where
    sumY = ds & fmap (integrate (NegInf `thru` PosInf)) & sum
    sumX = ds & fmap (contDom >>> \b -> hi b - lo b) & sum
    unFinite = \case Finite x -> x
