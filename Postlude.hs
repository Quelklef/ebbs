module Postlude
  ( spy
  , spyBoth
  , spyLeft
  , mtimes
  , (&)
  , unsafePerformIO
  , (>>>)
  , (<<<)
  , (%)
  , numerator
  , denominator
  , bool
  , catMaybes
  , toList
  ) where

import           Data.Function ((&))
import           System.IO.Unsafe (unsafePerformIO)
import           Control.Category ((>>>), (<<<))
import           Data.Ratio ((%), numerator, denominator)
import           Data.Bool (bool)
import           Data.Maybe (catMaybes)
import           Data.Foldable (toList)

spyBoth :: (Show a, Show b) => a -> b -> b
spyBoth a b = unsafePerformIO $ do
  putStrLn $ show a <> " | " <> show b
  pure b

spyLeft :: Show a => a -> b -> b
spyLeft a b = unsafePerformIO $ do
  putStrLn $ show a
  pure b

spy :: Show b => b -> b
spy b = unsafePerformIO $ do
  putStrLn $ show b
  pure b

mtimes :: (Monoid m, Integral n) => n -> m -> m
mtimes n m
    | n <= 0 = mempty
    | otherwise = m <> mtimes (n - 1) m
