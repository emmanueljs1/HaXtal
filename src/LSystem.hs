module LSystem where

import Prelude hiding (lookup)
import Test.QuickCheck
import Data.Map hiding (mapMaybe)
import Data.Monoid
import Data.Maybe
import Control.Monad

data Symbol = Forward
            | Turn Float
            | PushState
            | PopState
            | BOOP
            deriving (Eq, Ord, Show)

type Rules = Map Char String

type DrawRules = Map Char Symbol

data LSystem = LSystem {start :: String, rules :: Rules, toDraw :: DrawRules}

-- Takes an LSystem and produces an infinite list of iterative expansions.
-- The nth element of the result is the list of Symbols obtained from
-- expanding the LSystem n iterations.
expand :: LSystem -> [[Symbol]]
expand (LSystem initial rs td) =
  mapMaybe (`lookup` td) <$> iterate app initial
  where
    app = concatMap (\s -> findWithDefault [s] s rs)


makeDrawRule :: Char -> Symbol -> DrawRules
makeDrawRule = singleton

fDrawRule :: DrawRules
fDrawRule = makeDrawRule 'F' Forward

lBracketDrawRule :: DrawRules
lBracketDrawRule = makeDrawRule '[' PushState

rBracketDrawRule :: DrawRules
rBracketDrawRule = makeDrawRule ']' PopState

makePlusDrawRule :: Float -> DrawRules
makePlusDrawRule a = makeDrawRule '+' (Turn a)

makeMinusDrawRule :: Float -> DrawRules
makeMinusDrawRule a = makeDrawRule '-' (Turn a)

makeDefaultDrawRules :: Float -> DrawRules
makeDefaultDrawRules a = fDrawRule <> lBracketDrawRule <> rBracketDrawRule <>
                          makePlusDrawRule (2 * pi - a) <> makeMinusDrawRule a

makeRule :: Char -> String -> Rules
makeRule = singleton

instance Arbitrary LSystem where
  arbitrary = undefined
  shrink = undefined
