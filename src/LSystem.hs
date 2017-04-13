module LSystem where

import Test.QuickCheck

data Operator = Operator {name :: Char, op :: Op} deriving Eq

instance Show Operator where
  show (Operator n _)= [n]

data Op = Forward
        | Backward
        | Turn Float
        deriving (Show, Eq)

data LSystem = LSystem {start :: [Operator], rules :: Operator -> [Operator]}

sierpinski :: LSystem
sierpinski = LSystem [f, minus, g, minus, g] exec where
  exec o =
    case o of
      (Operator 'F' Forward)         -> [f, minus, g, plus, f, plus, g, minus, f]
      (Operator 'G' Forward)         -> [g, g]
      pm | pm == plus || pm == minus -> [pm]
      _                              -> []
  f = Operator 'F' Forward
  g = Operator 'G' Forward
  plus = Operator '+' (Turn 120)
  minus = Operator '-' (Turn 240)

expand :: LSystem -> Int -> [Operator]
expand (LSystem s r) = expand' s
  where
    expand' [] _ = []
    expand' ops 0 = ops
    expand' ops n = expand' (concatMap r ops) $ n-1
    


instance Arbitrary LSystem where
  arbitrary = undefined
  shrink = undefined
