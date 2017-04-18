module LSystem where

import Test.QuickCheck
import Data.Map
import Data.Monoid

data Symbol = Forward
            | Turn Double
            | Var Char
            deriving (Eq, Ord, Show)

type Rules = Map Symbol [Symbol]

--data Variable = Variable {rec :: [Variable], draw :: DrawOp}

data LSystem = LSystem {start :: [Symbol], rules :: Rules}

expand :: LSystem -> [[Symbol]]
expand (LSystem initial rules) = iterate app initial
  where
    app = concatMap (\s -> (findWithDefault [s] s rules))

makePlus :: Double -> Symbol
makePlus = Turn

makeMinus :: Double -> Symbol
makeMinus f = Turn (360 - f)

sierpinski' :: LSystem
sierpinski' = LSystem [f, x] (r1 <> r2 <> r3) where
  f = Forward
  x = Var 'X'
  y = Var 'Y'
  z = Var 'Z'
  r1 = singleton f [z]
  r2 = singleton x [plus, f, y, minus, f, x, minus, f, y, plus]
  r3 = singleton y [minus, f, x, plus, f, y, plus, f, x, minus]
  plus = makePlus 60
  minus = makeMinus 60

sierpinski = LSystem [Forward, f, minus, Forward, g, minus, Forward, g] (fRule <> gRule <> zR1) where
  f = Var 'F'
  g = Var 'G'
  fRule = singleton f [Forward, f, minus, Forward, g, plus, Forward, f, plus, Forward, g, minus, Forward, f]
  gRule = singleton g [Forward, g, Forward, g]
  zR1 = singleton Forward [Var 'Z']
  plus = makePlus 120
  minus = makeMinus 120

dragon :: LSystem
dragon = LSystem [Forward, x] (r1 <> r2) where
    x = Var 'X'
    y = Var 'Y'
    r1 = singleton x [x, plus, y, Forward, plus]
    r2 = singleton y [minus, Forward, x, minus, y]
    plus = makePlus 90
    minus = makeMinus 90
instance Arbitrary LSystem where
  arbitrary = undefined
  shrink = undefined
