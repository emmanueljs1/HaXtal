module LSystemTest where

import LSystem
import Test.HUnit
import Test.QuickCheck
import Data.Map
import Data.Maybe
import Prelude hiding (lookup)
import Data.Char

texpand :: Test
texpand =
  TestList [
            "expand no start" ~:
              expand (LSystem "" (makeRule 'A' "A") jumpDrawRule) !! 1 ~?= [],
            "expand no expand rules" ~:
              expand (LSystem "X" baseRule forwardDrawRule) !! 1 ~?= [],
            "expand no draw rules" ~:
              expand (LSystem "F" (makeRule 'F' "FF") baseDrawRule) !! 1 ~?= [],
            "expand valid lsys" ~:
              expand (LSystem "F" (makeRule 'F' "FF") forwardDrawRule) !! 1 ~?=
              [Forward, Forward]
           ]

tzipMap :: Test
tzipMap =
  TestList [
            "zipMap both nil" ~: zipMap ([] :: [Int]) ([] :: [String]) ~?=
              (empty :: Map Int String),
            "zipMap keys nil" ~: zipMap ([] :: [Int]) ["A"] ~?=
              (empty :: Map Int String),
            "zipMap values nil" ~: zipMap [1] ([] :: [String]) ~?=
              (empty :: Map Int String),
            "zipMap more keys than values" ~: zipMap [1, 2, 3] ["A"] ~?=
              insert 1 "A" empty,
            "zipMap more values than keys" ~: zipMap [1] ["A", "B"] ~?=
              insert 1 "A" empty,
            "zipMap same amt keys and vals" ~: zipMap [1, 2] ["A", "B"] ~?=
              insert 1 "A" (insert 2 "B" empty)
           ]

prop_validStart :: LSystem -> Bool
prop_validStart lsys = start lsys /= ""

prop_allVariablesMappedToRule :: LSystem -> Bool
prop_allVariablesMappedToRule lsys = check (start lsys) where
  check [] = True
  check (h : t)
    | isUpper h = isJust (lookup h (rules lsys)) && check t
    | otherwise = check t

main :: IO ()
main = do
  _ <- runTestTT texpand
  _ <- runTestTT tzipMap
  quickCheck prop_validStart
  quickCheck prop_allVariablesMappedToRule
  return ()
