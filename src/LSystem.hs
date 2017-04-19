module LSystem where

import Test.QuickCheck
import Data.Map
import Data.Monoid

data Symbol = Forward
            | LeftTurn
            | RightTurn
            | Var Char
            deriving (Eq, Ord, Show)

type Rules = Map Symbol [Symbol]

data LSystem = LSystem {start :: [Symbol], rules :: Rules, angle :: Double}

expand :: LSystem -> [[Symbol]]
expand (LSystem initial rs _) = iterate app initial
  where
    app = concatMap (\s -> findWithDefault [s] s rs)

toSymbol :: Char -> Symbol
toSymbol 'F' = Forward
toSymbol '+' = RightTurn
toSymbol '-' = LeftTurn
toSymbol  z = Var z

makeRule :: Char -> String -> Rules
makeRule c s = singleton (toSymbol c) $ toSymbol <$> s

makeStart :: String -> [Symbol]
makeStart = fmap toSymbol

sierpinski' :: LSystem
sierpinski' = LSystem (makeStart "FX") (r1 <> r2 <> r3) 60 where
  r1 = makeRule 'F' "Z"
  r2 = makeRule 'X' "+FY-FX-FY+"
  r3 = makeRule 'Y' "-FX+FY+FX-"

sierpinski :: LSystem
sierpinski = LSystem (makeStart "FA-FG-FG") (r1 <> r2 <> r3) 120 where
  r1 = makeRule 'A' "FA-FG+FA+FG-FA"
  r2 = makeRule 'G' "FGFG"
  r3 = makeRule 'F' "Z"

dragon :: LSystem
dragon = LSystem (makeStart "FX") (r1 <> r2) 90 where
  r1 = makeRule 'X' "X+YF+"
  r2 = makeRule 'Y' "-FX-Y"

hilbert :: LSystem
hilbert = LSystem (makeStart "X") (r1 <> r2) 90 where
  r1 = makeRule 'X' "-YF+XFX+FY-"
  r2 = makeRule 'Y' "+XF-YFY-FX+"

gosper :: LSystem
gosper = LSystem (makeStart "FA") (r1 <> r2 <> r3) 60 where
  r1 = makeRule 'A' "FA-FB--FB+FA++FAFA+FB-"
  r2 = makeRule 'B' "+FA-FBFB--FB-FA++FA+FB"
  r3 = makeRule 'F' "Z"

instance Arbitrary LSystem where
  arbitrary = undefined
  shrink = undefined
