-- | This file lays down a framework for representing LSystems
-- as Haskell Types
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
            | Jump        -- move pen forward without drawing
            | Turn Float  -- turn the given angle
            | AdjAngle Float    -- change global angle adjustment
            | AdjLen Float      -- change global line length
            | PushState   -- save the current position and angle
            | PopState    -- access the most recently pushed position and angle
            deriving (Eq, Ord, Show)

-- In our framework, a _variable_ or a _constant_ can have a
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

-- | Finally, an LSystem is a start state, a set of rules, and a set
-- of draw rules; an example LSystem could be:
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
  show l =
    intercalate "\n" [startLine, variables, angleLine, rulesText] where
      startLine = "Start: "     <> startString l
      variables = "Variables: " <> varsString  l
      angleLine = "Angle: "     <> angleString l
      rulesText = "Rules:\n"    <> rulesString l

-- | start state of LSystem as a string
startString :: LSystem -> String
startString = start

-- | rules of LSystem as strings
rulesString :: LSystem -> String
rulesString (LSystem _ rs _) =
  intercalate "\n" $ (\(k, v) -> [k] <> ": " <> v) <$> assocs rs

-- | angle of LSystem
angleString :: LSystem -> String
angleString (LSystem _ _ drs) =
  case lookup '-' drs of
    Just (Turn f) -> show $ toDeg f
    _ -> "90.0"
  where
    toDeg = (* 180.0) . (/ pi)

-- | variables of LSystem
varsString :: LSystem -> String
varsString (LSystem _ rs _) = intersperse ',' (keys rs)

-- | Takes an LSystem and produces an infinite list of iterative expansions.
-- The nth element of the result is the list of Symbols obtained from
-- expanding the LSystem n iterations.
expand :: LSystem -> [[Symbol]]
expand (LSystem initial rs td) =
  mapMaybe (`lookup` td) <$> iterate app initial
  where
    app = concatMap (\s -> findWithDefault [s] s rs)

-- | base draw rules (no rules at all)
baseDrawRule :: DrawRules
baseDrawRule = empty

-- | makes a draw rule (associates a character to a symbol)
makeDrawRule :: Char -> Symbol -> DrawRules
makeDrawRule = singleton

-- | combines two sets of draw rules into one set of draw rules
combineDrawRules :: DrawRules -> DrawRules -> DrawRules
combineDrawRules = (<>)

-- | forward draw rule, relates 'F' to Forward
forwardDrawRule :: DrawRules
forwardDrawRule = makeDrawRule 'F' Forward

-- | jump draw rule, relates 'f' to Jump
jumpDrawRule :: DrawRules
jumpDrawRule = makeDrawRule 'f' Jump

-- | push state draw rule, relates '[' to saving state
lBracketDrawRule :: DrawRules
lBracketDrawRule = makeDrawRule '[' PushState

-- | pop state draw rule, relates ']' to popping most recent state
rBracketDrawRule :: DrawRules
rBracketDrawRule = makeDrawRule ']' PopState

-- | plus draw rule, relates '+' to turning by an angle a
makePlusDrawRule :: Float -> DrawRules
makePlusDrawRule a = makeDrawRule '+' (Turn a)

-- | minus draw rule, relates '-' to turning by an angle a
makeMinusDrawRule :: Float -> DrawRules
makeMinusDrawRule a = makeDrawRule '-' (Turn a)

-- turn draw rules
makeTurnDrawRule :: Float -> DrawRules
makeTurnDrawRule a = makePlusDrawRule (-a) <> makeMinusDrawRule a

-- | increase angle draw rule, relates '<' to changing global angle
-- adjustment by an angle a
makeIncAngleRule :: Float -> DrawRules
makeIncAngleRule a = makeDrawRule '<' (AdjAngle a)

-- | decrease angle draw rule, relates '>' to changing global angle
-- adjustment by an angle a
makeDecAngleRule :: Float -> DrawRules
makeDecAngleRule a = makeDrawRule '>' (AdjAngle a)

-- | angle adjustment draw rules
makeAdjAngleRule :: Float -> DrawRules
makeAdjAngleRule a = makeIncAngleRule (-a) <> makeDecAngleRule a

-- increase line length draw rule, relates '(' to changing global line
-- length by a float a
makeIncLenRule :: Float -> DrawRules
makeIncLenRule a = makeDrawRule '(' (AdjLen a)

-- decrease line length draw rule, relates ')' to changing global
-- line length by a float a
makeDecLenRule :: Float -> DrawRules
makeDecLenRule a = makeDrawRule ')' (AdjLen a)

-- line length draw rules
makeAdjLenRule :: Float -> DrawRules
makeAdjLenRule a = makeIncLenRule a <> makeDecLenRule (-a)

-- | default draw rules are the most common rules, these relate
-- 'F' to Forward, '[' to push state, ']' to pop state, '+' and '-' to turning
-- by complementary angles
makeDefaultDrawRules :: Float -> DrawRules
makeDefaultDrawRules a = forwardDrawRule <> jumpDrawRule <>
                          lBracketDrawRule <> rBracketDrawRule <>
                          makeTurnDrawRule a

-- | Creates draw rules with the additional variables X and Y
additionalVariableDrawRules :: Float -> DrawRules
additionalVariableDrawRules f =
  insert 'X' Forward $ insert 'Y' Forward (makeDefaultDrawRules f)

-- | base rule (no rules)
baseRule :: Rules
baseRule = empty

-- | makes a rule
makeRule :: Char -> String -> Rules
makeRule = singleton

-- | combine two sets of rules into one set of rules
combineRules :: Rules -> Rules -> Rules
combineRules = (<>)

-- | zipMap keys values
-- takes a list of keys and a list of values and returns
-- a map that links each key in the first list to its
-- corresponding value in the second list
zipMap :: Ord k => [k] -> [v] -> Map k v
zipMap = foldr combine (const empty) where
  combine x acc (h : t) = insert x h (acc t)
  combine _ _ [] = empty

-- | arbitrary defined for an LSystem
-- the resize function actually determines which arbitrary instance is used!
-- resize 0 arbitrary is a regular LSystem that can use all constants
-- resize 1 arbitrary only uses turns
-- resuze 2 arbitrary only uses turns and pushes/pops
-- resize 3 arbitrary only uses turns and increase/decrease angles
-- there is also a 5% that the LSystem will include jumping in its rules
instance Arbitrary LSystem where
  arbitrary = sized chooseArbitraryLSystem where
    drawableFrequencies 1 =
      [
        (1, elements ['+', '-', 'F', 'X', 'Y'])
      ]
    drawableFrequencies 2 =
      [
        (3, elements ['+', '-', 'F', 'X', 'Y']),
        (1, elements ['[', ']'])
      ]
    drawableFrequencies 3 =
      [
        (3, elements ['+', '-', 'F', 'X', 'Y']),
        (1, elements ['(', ')'])
      ]
    drawableFrequencies _ =
      [
        (6, elements ['+', '-', 'F', 'X', 'Y']),
        (3, elements ['[', ']']),
        (1, elements ['(', ')'])
      ]
    chooseArbitraryLSystem i =
      liftM3 LSystem arbStart arbRules arbDrawRules where
        variables = ['F', 'X', 'Y']
        drawable =
          frequency [
                      (19, frequency (drawableFrequencies i)),
                      (1, elements ['f'])
                    ]
        combination = comb where
          chooseVariable = elements variables
          randomList = resize 10 $ listOf drawable
          comb = do
            l <- liftM2 (:) chooseVariable randomList
            shuffle l
        arbStart = combination
        combination1 = comb where
          chooseVariable = elements variables
          randomList = resize 10 $ listOf1 drawable
          comb = do
            l <- liftM2 (:) chooseVariable randomList
            shuffle l
        arbRules = do
          rulesToMap <- vectorOf 3 $ resize 5 combination1
          return $ zipMap variables rulesToMap
        arbDrawRules = do
          f <- choose (0, 2 * pi)
          len <- choose (0.1, 0.5)
          return (additionalVariableDrawRules f <> makeAdjLenRule len)

-- | holds the string representation of an LSystem
data LSysComps = LSysComps {lscStart :: String, lscRules :: String,
                            lscVars :: String, lscAngle :: String,
                            lscLevels :: String}

instance Monoid LSysComps where
  mempty = LSysComps "" "" "" "" ""
  mappend (LSysComps s1 r1 v1 a1 l1) (LSysComps s2 r2 v2 a2 l2) =
    LSysComps (t s1 s2) (t r1 r2) (t v1 v2) (t a1 a2) (t l1 l2)
    where
      t a b
        | b == "" = a
        | otherwise = b

-- | Gets an LSystem from a set of user strings
-- getLSystem start rules variables angle
-- _start_ is simply the starting string (all whitespaces are ignored)
-- _rules_ is a 'text box' of rules of this form:
--   X : XY+Y
--   Y : FYF
-- _variables_ are just the variables separated by ommas and/or
-- spaces and/or nothing
--   X, Y
--   X Y
--   XY
-- _angle_ is just the angle for the LSystem, if an angle is not successfully
-- read, the default value it is given is 90
getLSystem :: LSysComps -> LSystem
getLSystem (LSysComps st rs variables angle _) = LSystem st' recRules drawRules
  where
    st' = filter (not . isSpace) st
    recRules = parseLines empty allLines
    allLines = filter (not . isSpace) <$> lines rs
    parseLines m [] = m
    parseLines m (s : ss) =
      case s of
        (c : ':' : cs) -> parseLines (insert c cs m) ss
        _ -> parseLines m ss
    drawRules = getVariables variables'      <>
                defaultDrawRules             <>
                makeAdjAngleRule (pi / 2^16) <>
                makeAdjLenRule 0.1
    getVariables [] = empty
    getVariables ('f' : vs) = insert 'f' Jump (getVariables vs)
    getVariables (v : vs) = insert v Forward (getVariables vs)
    defaultDrawRules =
      case readMaybe angle of
        Just f -> makeDefaultDrawRules (toRad f)
        Nothing -> makeDefaultDrawRules $ toRad 90.0
    toRad = (/ 180.0) . (* pi)
    variables' =
      filter (\c -> not (isSpace c) && c /= ',') variables <> filter isUpper st
