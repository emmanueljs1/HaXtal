-- | This file lays down a framework for representing LSystems
-- | as Haskell Types

module LSystem where

import Prelude hiding (lookup)
import Test.QuickCheck
import Data.Map hiding (mapMaybe, foldr, filter)
import Data.Monoid
import Data.Maybe
import Data.List hiding (lookup, insert)
import Data.Char
import Text.Read
import Control.Monad

-- | A symbol represents an action to be performed
data Symbol = Forward     -- draw forward
            | Turn Float  -- turn the current angle
            | PushState   -- save the current position
            | PopState    -- access the most recently pushed position
            | BOOP        -- TODO: what is BOOP
            deriving (Eq, Ord, Show)

-- | In our framework, a _variable_ or a _constant_ can have a
-- symbol associated with it.
-- The difference between a variable and a constant is that a variable
-- has a rule that defines how it is replaced, while a constant does not

-- | Rules define the way a variable can be replaced with a combination of
-- variables and constants.
-- We represent them as a map from characters to strings.
-- For example X -> XX could be represented in the map as ('X', "XX")
type Rules = Map Char String

-- | Draw rules map variables and constants to symbols
type DrawRules = Map Char Symbol

-- | Finally, an LSystem is a start state, a set of rules, and a set of draw rules
-- an example LSystem could be:
--   variables: F
--   constants: +
--   start: F
--   rules: F -> F + F
--   draw rules: F -> Forward; + -> Turn 90
data LSystem = LSystem { start :: String,
                         rules :: Rules,
                         toDraw :: DrawRules }
                       deriving Eq

instance Show LSystem where
  show (LSystem s rs drs) =
    intercalate "\n" [startLine, variables, angleLine, rulesText] where
      startLine = "Start: " <> s
      variables = "Variables: " <> intersperse ',' (keys rs)
      angleLine = "Angle: " <> angle where
        angle =
          case lookup '-' drs of
            Just (Turn f) -> show f
            _ -> "90.0"
      rulesText = intercalate "\n" ("Rules:" : rulesLines) where
        rulesLines = (\(k, v) -> [' ', k] <> ": " <> v) <$> assocs rs

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

-- zipWithMap keys values
-- takes a list of keys and a list of values and returns
-- a map that links each key in the first list to its
-- corresponding value in the second list
zipWithMap :: Ord k => [k] -> [v] -> Map k v
zipWithMap = foldr combine (const empty) where
  combine x acc (h : t) = insert x h (acc t)
  combine _ _ [] = empty

instance Arbitrary LSystem where
  arbitrary = liftM3 LSystem arbStart arbRules arbDrawRules where
    variables = ['F', 'X', 'Y']
    combination = comb where
      chooseVariable = elements variables
      randomList = resize 10 . listOf $ elements ['F', 'X', 'Y', '+', '-']
      comb = do
        l <- liftM2 (:) chooseVariable randomList
        shuffle l
    arbStart = combination
    arbRules = do
      rulesToMap <- vectorOf 3 $ resize 5 combination
      return $ zipWithMap variables rulesToMap
    arbDrawRules = do
      f <- elements [1.0..360.0]
      return $ insert 'Y' Forward $ insert 'X' Forward (makeDefaultDrawRules f)
  shrink = undefined

-- Gets an LSystem from a set of user strings
-- getLSystem start rules variables angle
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
getLSystem :: [String] -> LSystem
getLSystem (st : rs : variables : angle : []) = LSystem st' recRules drawRules where
  st' = filter (not . isSpace) st
  recRules = parseLines empty allLines where
    allLines = filter (not . isSpace) <$> lines rs
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
    variables' = filter (\c -> not (isSpace c) && c /= ',') variables
getLSystem _ = LSystem "" baseRule baseDrawRule
