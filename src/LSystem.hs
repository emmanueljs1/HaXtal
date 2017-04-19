module LSystem where

import Prelude hiding (lookup)
import Test.QuickCheck
import Data.Map hiding (mapMaybe)
import Data.Monoid
import Data.Maybe
import Control.Monad

data Symbol = Forward
            | LeftTurn
            | RightTurn
            deriving (Eq, Ord, Show)

type Rules = Map Char String

type DrawRules = Map Char Symbol

data LSystem = LSystem {start :: String, rules :: Rules,
 toDraw :: DrawRules, angle :: Double}

expand :: LSystem -> [[Symbol]]
expand (LSystem initial rs td _) =
  mapMaybe (`lookup` td) <$> iterate app initial
  where
    app = concatMap (\s -> findWithDefault [s] s rs)


makeDrawRule :: Char -> Symbol -> DrawRules
makeDrawRule = singleton

fDrawRule :: DrawRules
fDrawRule = makeDrawRule 'F' Forward

plusDrawRule :: DrawRules
plusDrawRule = makeDrawRule '+' RightTurn

minusDrawRule :: DrawRules
minusDrawRule = makeDrawRule '-' LeftTurn

defaultDrawRules :: DrawRules
defaultDrawRules = fDrawRule <> plusDrawRule <> minusDrawRule

makeRule :: Char -> String -> Rules
makeRule = singleton

sierpinski :: LSystem
sierpinski = LSystem "A-G-G" (r1 <> r2)
             (defaultDrawRules <> dr1 <> dr2) 120 where
  r1 = makeRule 'A' "A-G+A+G-A"
  r2 = makeRule 'G' "GG"
  dr1 = makeDrawRule 'A' Forward
  dr2 = makeDrawRule 'G' Forward

sierpinskiArrowhead :: LSystem
sierpinskiArrowhead =
  LSystem "A" (r1 <> r2) (defaultDrawRules <> dr1 <> dr2) 60 where
  r1 = makeRule 'A' "+B-A-B+"
  r2 = makeRule 'B' "-A+B+A-"
  dr1 = makeDrawRule 'A' Forward
  dr2 = makeDrawRule 'B' Forward

dragon :: LSystem
dragon = LSystem "FX" (r1 <> r2) defaultDrawRules 90 where
  r1 = makeRule 'X' "X+YF+"
  r2 = makeRule 'Y' "-FX-Y"

hilbert :: LSystem
hilbert = LSystem "X" (r1 <> r2) defaultDrawRules 90 where
  r1 = makeRule 'X' "-YF+XFX+FY-"
  r2 = makeRule 'Y' "+XF-YFY-FX+"

gosper :: LSystem
gosper = LSystem "A" (r1 <> r2)
         (defaultDrawRules <> dr1 <> dr2) 60 where
  r1 = makeRule 'A' "A-B--B+A++AA+B-"
  r2 = makeRule 'B' "+A-BB--B-A++A+B"
  dr1 = makeDrawRule 'A' Forward
  dr2 = makeDrawRule 'B' Forward

instance Arbitrary LSystem where
  arbitrary = undefined
  shrink = undefined
