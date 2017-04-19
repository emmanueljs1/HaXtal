module LSystem where

import Prelude hiding (lookup)
import Test.QuickCheck
import Data.Map hiding (mapMaybe)
import Data.Monoid
import Data.Maybe

data Symbol = Forward
            | LeftTurn
            | RightTurn
            deriving (Eq, Ord, Show)

type Rules = Map Char String

type DrawRules = Map Char Symbol

data LSystem = LSystem {start :: String, rules :: Rules, toDraw :: DrawRules, angle :: Double}

expand :: LSystem -> [[Symbol]]
expand (LSystem initial rs td _) = mapMaybe (`lookup` td) <$> iterate app initial
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

makeStart :: String -> String
makeStart = id

sierpinski' :: LSystem
sierpinski' = LSystem (makeStart "X") (r1 <> r2) (defaultDrawRules <> dr1 <> dr2) 60 where
  r1 = makeRule 'X' "+Y-X-Y+"
  r2 = makeRule 'Y' "-X+Y+X-"
  dr1 = makeDrawRule 'X' Forward
  dr2 = makeDrawRule 'Y' Forward

-- sierpinski :: LSystem
-- sierpinski = LSystem (makeStart "FA-FG-FG") (r1 <> r2) 120 where
--   r1 = makeRule 'A' "A-G+A+G-A"
--   r2 = makeRule 'G' "GG"
--   toSymbol 'A' = Forward
--   toSymbol 'G' = Forward
--   -- r3 = makeRule 'F' "Z"

-- dragon :: LSystem
-- dragon = LSystem (makeStart "FX") (r1 <> r2) 90 where
--   r1 = makeRule 'X' "X+YF+"
--   r2 = makeRule 'Y' "-FX-Y"

-- hilbert :: LSystem
-- hilbert = LSystem (makeStart "X") (r1 <> r2) 90 where
--   r1 = makeRule 'X' "-YF+XFX+FY-"
--   r2 = makeRule 'Y' "+XF-YFY-FX+"

-- gosper :: LSystem
-- gosper = LSystem (makeStart "FA") (r1 <> r2 <> r3) 60 where
--   r1 = makeRule 'A' "FA-FB--FB+FA++FAFA+FB-"
--   r2 = makeRule 'B' "+FA-FBFB--FB-FA++FA+FB"
--   r3 = makeRule 'F' "Z"

instance Arbitrary LSystem where
  arbitrary = undefined
  shrink = undefined
