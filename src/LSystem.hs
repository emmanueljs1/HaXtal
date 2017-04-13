module LSystem where

import Test.QuickCheck

data RecOp = NOPr
           | Replicate Int
           | Custom [Variable]

data DrawOp = NOPd
            | Forward
            | Backward
            | Turn Float
            deriving (Show, Eq)

data Variable = Variable {rec :: RecOp, draw :: DrawOp}

newtype LSystem = LSystem {start :: [Variable]}

class Recursive a where
    recurse :: a -> [a]
    recurseAll :: [a] -> [a]
    recurseAll = concatMap recurse
    recurseAllN :: Int -> [a] -> [a]
    recurseAllN 0 l = l
    recurseAllN n l = recurseAllN (n - 1) (recurseAll l)

instance Recursive Variable where
    recurse v@(Variable NOPr _)           = [v]
    recurse v@(Variable (Replicate n) _) = replicate n v
    recurse (Variable (Custom vs) _)     = vs

makePlus :: Float -> Variable
makePlus f = Variable NOPr (Turn f)

makeMinus :: Float -> Variable
makeMinus f = Variable NOPr (Turn (360 - f))

forward :: Variable
forward = Variable NOPr Forward

sierpinski :: LSystem
sierpinski = LSystem [f, minus, g, minus, g] where
  f = Variable (Custom [f, minus, g, plus, f, plus, g, minus, f]) Forward
  g = Variable (Replicate 2) Forward
  plus = makePlus 120
  minus = makeMinus 120

dragon :: LSystem
dragon = LSystem [forward, x] where
    x = Variable (Custom [x, plus, y, forward, plus]) NOPd
    y = Variable (Custom [minus, forward, x, minus, y]) NOPd
    plus = makePlus 90
    minus = makeMinus 90

    


instance Arbitrary LSystem where
  arbitrary = undefined
  shrink = undefined
