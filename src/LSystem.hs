module LSystem where

import Test.QuickCheck

data Operator = Operator {name :: Char, op :: Op}

data Op = Forward
        | Backward
        | Right
        | Left
        | Turn Int

newtype LSystem = LSystem {rules :: Operator -> [Operator]}

sierpinski o =
  case o of
    (Operator 'F' Forward) -> [f, minus, g, plus, f, plus, g, minus, f]
    (Operator 'G' Forward) -> [g, g]
    _                      -> []
  where
    f = Operator 'F' Forward
    g = Operator 'G' Forward
    plus = Operator '+' (Turn 120)
    minus = Operator '-' (Turn $ -120)

instance Arbitrary LSystem where
  arbitrary = undefined
  shrink = undefined
