-- | Code for representing an LSystem as a list of drawable points
module Draw where

import LSystem
import Control.Monad.Trans.State.Lazy
import Data.Monoid hiding (getAll)

-- | represents what a given point should be drawn as
data DrawCommand = NOP | Pt | NewLine deriving (Show, Eq)

-- | represents a bounding box over a shape
data BoundingBox =
  BoundingBox {
    minX :: Float,
    minY :: Float,
    maxX :: Float,
    maxY :: Float
  } deriving (Show, Eq)

-- | makes a bounding box for one point
makeBoundingBox :: Point -> BoundingBox
makeBoundingBox p = BoundingBox x y x y
                    where
                      x = getX p
                      y = getY p

instance Monoid BoundingBox where
  mempty        = BoundingBox maxValue maxValue minValue minValue
  mappend b1 b2 =
    BoundingBox (min (minX b1) (minX b2)) (min (minY b1) (minY b2))
                (max (maxX b1) (maxX b2)) (max (maxY b1) (maxY b2))

class Vector a where
  makeV :: (Float, Float) -> a
  getX :: a -> Float
  getY :: a -> Float
  getP :: a -> (Float, Float)
  (>+) :: Vector b => a -> b -> a
  v1 >+ v2 =  makeV (getX v1 + getX v2, getY v1 + getY v2)
  rotateV :: Float -> a -> a
  rotateV r v = makeV (x * cos r - y * sin r, x * sin r + y * cos r)
                where
                  x = getX v
                  y = getY v
  mulSV :: Float -> a -> a
  mulSV s v = makeV (s * getX v, s * getY v)

newtype Direction = Direction (Float, Float) deriving (Show, Eq)

instance Vector Direction where
  makeV = Direction
  getX (Direction d) = fst d
  getY (Direction d) = snd d
  getP (Direction d) = d

newtype Point = Point (Float, Float) deriving (Show, Eq)

instance Vector Point where
  makeV = Point
  getX (Point p) = fst p
  getY (Point p) = snd p
  getP (Point p) = p

-- | Gets bounding box of a drawing for use in fitting the drawing in a display
getDrawBounds :: [Point] -> BoundingBox
getDrawBounds = foldr ((<>) . makeBoundingBox) mempty

-- | Gets vector paths that represent visualization of an LSystem
-- based on an input depth that determines the levels of recursion
getPaths :: Int -> LSystem -> [[Point]]
getPaths depth lsys = splitLines $ start : rest where
  start = (Pt, Point (0.0, 0.0))
  rest = getAll (expand lsys !! depth) initVector
  initVector = [(Point (0.0, 0.0), Direction (0.0, -1.0), 0, 1)]

-- | gets list of all points from list of symbols
getAll :: [Symbol] -> [(Point, Direction, Float, Float)] -> [(DrawCommand, Point)]
getAll [] _        = []
getAll (s : ss) st = val : getAll ss nst
                     where
                       (val, nst) = runState (getNext s) st

-- | splits into two lists of points, separated
-- determined by flag point
getNextLine :: [(DrawCommand, Point)] -> ([Point], [(DrawCommand, Point)])
getNextLine []                 = ([], [])
getNextLine ((dc, p) : ps) | dc == NOP     = (ln, remPts)
                           | dc == Pt      = (p : ln, remPts)
                           | dc == NewLine = ([] , (Pt, p) : ps)
                           where
                             (ln, remPts) = getNextLine ps

-- | splits list of (DrawCommand, Points) into separate lines for drawing
splitLines :: [(DrawCommand, Point)] -> [[Point]]
splitLines []  = []
splitLines pts = ln : splitLines remPts
                where
                  (ln, remPts) = getNextLine pts

-- | get next state based on current symbol
getNext :: Symbol -> State [(Point, Direction, Float, Float)] (DrawCommand, Point)
getNext Forward    = do
  ((curPos, curVec, adjAngle, linLen) : states) <- get
  let newPos = curPos >+ mulSV linLen curVec
  put ((newPos, curVec, adjAngle, linLen) : states)
  return (Pt, newPos)
getNext Jump       = do
  ((curPos, curVec, adjAngle, linLen) : states) <- get
  let newPos = curPos >+ mulSV linLen curVec
  put ((newPos, curVec, adjAngle, linLen) : states)
  return (NewLine, newPos)
getNext (Turn a)   = do
  ((curPos, curVec, adjAngle, linLen) : states) <- get
  let newVec = rotateV (a + adjAngle) curVec
  put ((curPos, newVec, adjAngle, linLen) : states)
  return (NOP, curPos)
getNext (AdjAngle a) = do
  ((curPos, curVec, adjAngle, linLen) : states) <- get
  let newAngle = adjAngle + a
  put ((curPos, curVec, newAngle, linLen) : states)
  return (NOP, curPos)
getNext (AdjLen a)   = do
  ((curPos, curVec, adjAngle, linLen) : states) <- get
  let newLen = linLen + a
  put ((curPos, curVec, adjAngle, newLen) : states)
  return (NOP, curPos)
getNext PushState    = do
  (s@(curPos, _, _, _) : states) <- get
  put (s : s : states)
  return (NOP, curPos)
getNext PopState     = do
  stateL <- get
  case stateL of
    (_ : s@(curPos, _, _, _) : states) -> do
      put (s : states)
      return $ (NewLine, curPos)
    (s@(curPos, _, _, _) : states) -> do
      put (s : states)
      return (NOP, curPos)

-- from Numeric.Limits source code (package cannot be found
-- using the reflex-platform)

-- | The maximum finite value for the type.
{-# SPECIALIZE maxValue :: Double #-}
{-# SPECIALIZE maxValue :: Float #-}
maxValue :: (RealFloat a) => a
maxValue = x
  where n = floatDigits x
        b = floatRadix x
        (_, u) = floatRange x
        x = encodeFloat (b^n - 1) (u - n)

-- | The minimum (positive) normalized value for the type.
{-# SPECIALIZE minValue :: Double #-}
{-# SPECIALIZE minValue :: Float #-}
minValue :: (RealFloat a) => a
minValue = x
  where n = floatDigits x
        b = floatRadix x
        (l, _) = floatRange x
        x = encodeFloat (b^n - 1) (l - n - 1)
