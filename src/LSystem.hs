module LSystem where

import Test.QuickCheck
import Control.Monad

data DrawOp = NOP
            | Forward
            | Turn Float
            deriving (Show, Eq)

data Variable = Variable {rec :: [Variable], draw :: DrawOp}

newtype LSystem = LSystem {start :: [Variable]}

class Recursive a where
    recurse :: a -> [a]
    recurseAll :: [a] -> [a]
    recurseAll = concatMap recurse
    recurseAllN :: Int -> [a] -> [a]
    recurseAllN 0 l = l
    recurseAllN n l = recurseAllN (n - 1) (recurseAll l)

instance Recursive Variable where
    recurse v@(Variable [] _)           = [v]
    recurse (Variable vs _)     = vs

makePlus :: Float -> Variable
makePlus f = Variable [] (Turn f)

makeMinus :: Float -> Variable
makeMinus f = Variable [] (Turn (360 - f))

forward :: Variable
forward = Variable [] Forward

sierpinski :: LSystem
sierpinski = LSystem [f, minus, g, minus, g] where
  f = Variable [f, minus, g, plus, f, plus, g, minus, f] Forward
  g = Variable [g, g] Forward
  plus = makePlus 120
  minus = makeMinus 120

dragon :: LSystem
dragon = LSystem [forward, x] where
    x = Variable [x, plus, y, forward, plus] NOP
    y = Variable [minus, forward, x, minus, y] NOP
    plus = makePlus 90
    minus = makeMinus 90

instance Arbitrary DrawOp where
  arbitrary = oneof [elements [NOP, Forward], Turn <$> arbitrary]
  shrink = undefined

instance Arbitrary Variable where
  arbitrary = liftM2 Variable (listOf arbitrary) arbitrary
  shrink = undefined

instance Arbitrary LSystem where
  arbitrary = LSystem <$> arbitrary
  shrink = undefined
