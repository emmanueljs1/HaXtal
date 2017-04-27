module LSystemTest where

import LSystem
import Test.HUnit

testExpand :: Test
testExpand =
  TestList [
            "no start" ~: expand (LSystem "" baseRule baseDrawRule) ~?= []
           ]

main :: IO ()
main = undefined
