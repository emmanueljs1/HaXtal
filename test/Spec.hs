-- | Test everything
module Main where

import qualified DrawTest as DrawTest
import qualified LSystemTest as LSystemTest


main :: IO ()
main = do
  DrawTest.main
  LSystem.main
