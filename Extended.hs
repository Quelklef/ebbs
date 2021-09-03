module Extended where

import Postlude

import Pretty

data Extended n
  = Finite n
  | PosInf
  | NegInf
  deriving (Eq, Show)

instance (Pretty n) => Pretty (Extended n) where
  pretty (Finite n) = pretty n
  pretty PosInf = "+∞"
  pretty NegInf = "-∞"

instance Functor Extended where
  fmap f (Finite x) = Finite (f x)
  fmap _ PosInf = PosInf
  fmap _ NegInf = NegInf

instance (Eq n, Ord n) => Ord (Extended n) where
  NegInf <= _ = True
  _ <= NegInf = False
  
  _ <= PosInf = True
  PosInf <= _ = False
  
  Finite a <= Finite b = a <= b

-- Check this shit out
cmpZero :: (Eq n, Num n) => n -> Ordering
cmpZero x
  | x == 0 = EQ
  | x == abs x = GT
  | otherwise = LT

instance (Eq n, Num n) => Num (Extended n) where
  -- (+)

  Finite a + Finite b = Finite (a + b)
  
  NegInf + Finite _ = NegInf
  Finite _ + NegInf = NegInf
  NegInf + NegInf = NegInf
  
  PosInf + Finite _ = PosInf
  Finite _ + PosInf = PosInf
  PosInf + PosInf = PosInf
  
  NegInf + PosInf = undefined
  PosInf + NegInf = undefined

  -- (*)
  
  Finite a * Finite b = Finite (a * b)
  
  NegInf * Finite x =
    case cmpZero x of
      LT -> PosInf
      EQ -> undefined
      GT -> NegInf
  Finite x * NegInf = NegInf * Finite x
  NegInf * NegInf = PosInf
  
  PosInf * Finite x =
    case cmpZero x of
      LT -> NegInf
      EQ -> undefined
      GT -> PosInf
  Finite x * PosInf = PosInf * Finite x
  PosInf * PosInf = PosInf
  
  PosInf * NegInf = NegInf
  NegInf * PosInf = NegInf

  -- negate
  negate PosInf = NegInf
  negate NegInf = PosInf
  negate (Finite x) = Finite (negate x)

  -- abs
  abs PosInf = PosInf
  abs NegInf = PosInf
  abs (Finite x) = Finite (abs x)

  -- signum
  signum PosInf = Finite 1
  signum NegInf = Finite (-1)
  signum (Finite x) = Finite (signum x)

  -- fromInteger
  fromInteger = Finite . fromInteger
