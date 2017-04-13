module LSystem where

import Test.QuickCheck

data Operator = Operator {name :: Char, op :: Op}

data Op = Forward
        | Backward
        | Turn Int

data LSystem = LSystem {start :: [Operator], rules :: Operator -> [Operator]}

sierpinski :: LSystem
sierpinski = LSystem [f, minus, g, minus, g] expand where
  expand o =
    case o of
      (Operator 'F' Forward) -> [f, minus, g, plus, f, plus, g, minus, f]
      (Operator 'G' Forward) -> [g, g]
      _                      -> []
  f = Operator 'F' Forward
  g = Operator 'G' Forward
  plus = Operator '+' (Turn 120)
  minus = Operator '-' (Turn $ -120)

instance Arbitrary LSystem where
  arbitrary = undefined
  shrink = undefined
