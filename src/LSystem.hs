module LSystem where

import Test.QuickCheck

data RecOp = NOP
           | Replicate Int
           | Custom [Variable]

data DrawOp = Forward
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
    recurse v@(Variable NOP _)           = [v]
    recurse v@(Variable (Replicate n) _) = replicate n v
    recurse (Variable (Custom vs) _)     = vs

sierpinski :: LSystem
sierpinski = LSystem [f, minus, g, minus, g, g, minus, f, minus, g, g, minus, f, minus, g] where
  f = Variable (Custom [f, minus, g, plus, f, plus, g, minus, f]) Forward
  g = Variable (Replicate 2) Forward
  plus = Variable NOP (Turn 120)
  minus = Variable NOP (Turn 240)
    


instance Arbitrary LSystem where
  arbitrary = undefined
  shrink = undefined
