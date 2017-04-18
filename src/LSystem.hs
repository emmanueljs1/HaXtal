module LSystem where

import Test.QuickCheck
import Data.Map

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

sierpinski :: LSystem
sierpinski = LSystem [f, minus, g, minus, g] (fRule <> gRule) where
  f = Var 'F'
  g = Var 'G'
  fRule = singleton f [Forward, f, minus, g, plus, Forward, f, plus, g, minus, Forward, f]
  gRule = singleton [Forward, Forward, g, g]
  plus = makePlus 120
  minus = makeMinus 120
{-
dragon :: LSystem
dragon = LSystem [forward, x] where
    x = Variable [x, plus, y, forward, plus] NOP
    y = Variable [minus, forward, x, minus, y] NOP
    plus = makePlus 90
    minus = makeMinus 90
-}
instance Arbitrary LSystem where
  arbitrary = undefined
  shrink = undefined
