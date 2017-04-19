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
expand (LSystem initial rules _) = iterate app initial
  where
    app = concatMap (\s -> (findWithDefault [s] s rules))

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
sierpinski' = LSystem (makeStart "FX") (r1 <> r2 <> r3) 60.0 where
  r1 = makeRule 'F' "Z"
  r2 = makeRule 'X' "+FY-FX-FY+"
  r3 = makeRule 'Y' "-FX+FY+FX-"

sierpinski = LSystem [Forward, f, minus, Forward, g, minus, Forward, g] (fRule <> gRule <> zR1) 120.0 where
  f = Var 'F'
  g = Var 'G'
  fRule = singleton f [Forward, f, minus, Forward, g, plus, Forward, f, plus, Forward, g, minus, Forward, f]
  gRule = singleton g [Forward, g, Forward, g]
  zR1 = singleton Forward [Var 'Z']
  plus = RightTurn
  minus = LeftTurn

dragon :: LSystem
dragon = LSystem [Forward, x] (r1 <> r2) 90 where
    x = Var 'X'
    y = Var 'Y'
    r1 = singleton x [x, plus, y, Forward, plus]
    r2 = singleton y [minus, Forward, x, minus, y]
    plus = RightTurn
    minus = LeftTurn
instance Arbitrary LSystem where
  arbitrary = undefined
  shrink = undefined
