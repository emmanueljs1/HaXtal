-- | This file lays down a framework for representing LSystems
-- | as Haskell Types

module LSystem where

import Prelude hiding (lookup)
import Test.QuickCheck
import Data.Map hiding (mapMaybe)
import Data.Monoid
import Data.Maybe
import Data.Char
import Text.Read
import Control.Monad

-- a symbol represents an action to be performed
data Symbol = Forward     -- draw forward
            | Turn Float  -- turn the current angle
            | PushState   -- save the current position
            | PopState    -- access the most recently pushed position
            | BOOP        -- TODO: what is BOOP
            deriving (Eq, Ord, Show)

{- in our framework, a _variable_ or a _constant_ can have a
-- symbol associated with it

-- the difference between a variable and a constant is that a variable
-- has a rule that defines how it is replaced, while a constant does not -}

-- rules define the way a variable can be replaced with a combination of
-- variables and constants
-- we represent them as a map from characters to strings
-- for example X -> XX could be represented in the map as ('X', "XX")
type Rules = Map Char String

-- draw rules map variables and constants to symbols
type DrawRules = Map Char Symbol

-- finally, an LSystem is a start state, a set of rules, and a set of draw rules
-- an example LSystem could be:
--   variables: F
--   constants: +
--   start: F
--   rules: F -> F + F
--   draw rules: F -> Forward; + -> Turn 90
data LSystem = LSystem { start :: String,
                         rules :: Rules,
                         toDraw :: DrawRules }
                       deriving (Show, Eq)

-- Takes an LSystem and produces an infinite list of iterative expansions.
-- The nth element of the result is the list of Symbols obtained from
-- expanding the LSystem n iterations.
expand :: LSystem -> [[Symbol]]
expand (LSystem initial rs td) =
  mapMaybe (`lookup` td) <$> iterate app initial
  where
    app = concatMap (\s -> findWithDefault [s] s rs)

-- base draw rules (no rules at all)
baseDrawRule :: DrawRules
baseDrawRule = empty

-- makes a draw rule (associates a character to a symbol)
makeDrawRule :: Char -> Symbol -> DrawRules
makeDrawRule = singleton

-- combines two sets of draw rules into one set of draw rules
combineDrawRules :: DrawRules -> DrawRules -> DrawRules
combineDrawRules = (<>)

-- forward draw rule, relates 'F' to Forward
fDrawRule :: DrawRules
fDrawRule = makeDrawRule 'F' Forward

-- push state draw rule, relates '[' to saving state
lBracketDrawRule :: DrawRules
lBracketDrawRule = makeDrawRule '[' PushState

-- pop state draw rule, relates ']' to popping most recent state
rBracketDrawRule :: DrawRules
rBracketDrawRule = makeDrawRule ']' PopState

-- plus draw rule, relates '+' to turning by an angle a
makePlusDrawRule :: Float -> DrawRules
makePlusDrawRule a = makeDrawRule '+' (Turn a)

-- minus draw rule, relates '-' to turning by an angle a
makeMinusDrawRule :: Float -> DrawRules
makeMinusDrawRule a = makeDrawRule '-' (Turn a)

-- default draw rules are the most common rules, these relate
-- 'F' to Forward, '[' to push state, ']' to pop state, '+' and '-' to turning
-- by complementary angles
makeDefaultDrawRules :: Float -> DrawRules
makeDefaultDrawRules a = fDrawRule <> lBracketDrawRule <> rBracketDrawRule <>
                          makePlusDrawRule (2 * pi - a) <> makeMinusDrawRule a

-- base rule (no rules)
baseRule :: Rules
baseRule = empty

-- makes a rule
makeRule :: Char -> String -> Rules
makeRule = singleton

-- combine two sets of rules into one set of rules
combineRules :: Rules -> Rules -> Rules
combineRules = (<>)

instance Arbitrary LSystem where
  arbitrary = liftM3 LSystem arbStart arbRules arbDrawRules where
    variables = elements ['F', 'X', 'Y']
    randomList =
      resize 10 . listOf $ elements ['F', 'X', 'Y', '+', '-']
    combination = liftM2 (:) variables randomList
    arbStart = combination
    arbRules =
      Prelude.foldr (<>) empty .
      zipWith (\var comb -> insert var comb empty) ['F', 'X', 'Y'] <$>
      vectorOf 3 combination
    arbDrawRules = insert 'Y' Forward . insert 'X' Forward <$> ddr where
      ddr = makeDefaultDrawRules <$> elements [0.0..360.0]
  shrink = undefined

-- Parses an LSystem from a set of user strings
-- parseLSystem start rules variables angle
-- _start_ is simply the starting string (all whitespaces are ignored)
-- _rules_ is a 'text box' of rules of this form:
--   X : XY+Y
--   Y : FYF
-- _variables_ are just the variables separated by
-- _commas_ and/or spaces and/or nothing
--   X, Y
--   X Y
--   XY
-- _angle_ is just the angle for the LSystem, if an angle is not successfully
-- read, the default value it is given is 90
parseLSystem :: String -> String -> String -> String -> LSystem
parseLSystem st rules variables angle = LSystem st' recRules drawRules where
  st' = Prelude.filter (not . isSpace) st
  recRules = parseLines empty allLines where
    allLines = Prelude.filter (not . isSpace) <$> lines rules
    parseLines m [] = m
    parseLines m (s : ss) =
      case s of
        (c : ':' : cs) -> parseLines (insert c cs m) ss
        _ -> parseLines m ss
  drawRules = getVariables variables' <> defaultDrawRules where
    getVariables [] = empty
    getVariables (v : vs) = insert v Forward (getVariables vs)
    defaultDrawRules =
      case readMaybe angle of
        Just f -> makeDefaultDrawRules f
        Nothing -> makeDefaultDrawRules 90.0
    variables' = Prelude.filter (\c -> not (isSpace c) && c /= ',') variables
