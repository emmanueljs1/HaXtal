module Draw where

import LSystem
import Control.Monad.Trans.State.Lazy
import Data.Monoid hiding (getAll)
-- import Numeric.Limits

data BoundingBox =
  BoundingBox {
    minX :: Float,
    minY :: Float,
    maxX :: Float,
    maxY :: Float
  } deriving (Show, Eq)

makeBoundingBox :: Point -> BoundingBox
makeBoundingBox p = BoundingBox x y x y
                    where
                      x = getX p
                      y = getY p

instance Monoid BoundingBox where
  mempty        = BoundingBox maxValue maxValue minValue minValue
  mappend b1 b2 = BoundingBox (min (minX b1) (minX b2)) (min (minY b1) (minY b2))
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


getDrawBounds :: [Point] -> BoundingBox
getDrawBounds = foldr ((<>) . makeBoundingBox) mempty

-- Gets vector paths that represent visualization of an LSystem
getPaths :: Int -> LSystem -> [[Point]]
getPaths depth lsys = removeBad $ start : rest where
  start = Point (0.0, 0.0)
  rest = getAll (expand lsys !! depth) initVector
  initVector = [(Point (0.0, 0.0), Direction (0.0, -1.0), 0, 1)]

--
getAll :: [Symbol] -> [(Point, Direction, Float, Float)] -> [Point]
getAll [] _        = []
getAll (s : ss) st = val : getAll ss nst
                     where
                       (val, nst) = runState (getNext s) st


-- can be used for rounding angles when 90deg,
-- e.g. mapTuple (fromIntegral . round) $ rotateV (pi / 2) curVec
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

--
getNextLine :: [Point] -> ([Point], [Point])
getNextLine []            = ([], [])
getNextLine (p : ps)
              | getX p < -5000 = ([], p >+ Point (10000, 10000) : ps)
              | otherwise     = (p : ln, remPts)
              where
                (ln, remPts) = getNextLine ps

--
removeBad :: [Point] -> [[Point]]
removeBad []  = []
removeBad pts = ln : removeBad remPts
                where
                  (ln, remPts) = getNextLine pts

--
getNext :: Symbol -> State [(Point, Direction, Float, Float)] Point
getNext Forward        = do ((curPos, curVec, adjAngle, linLen) : states) <- get
                            let newPos = curPos >+ (mulSV linLen curVec)
                            put ((newPos, curVec, adjAngle, linLen) : states)
                            return newPos
getNext (Turn a)       = do ((curPos, curVec, adjAngle, linLen) : states) <- get
                            let newVec = rotateV (a + adjAngle) curVec
                            put ((curPos, newVec, adjAngle, linLen) : states)
                            return curPos
getNext (AdjAngle a)   = do ((curPos, curVec, adjAngle, linLen) : states) <- get
                            let newAngle = adjAngle + a
                            put ((curPos, curVec, newAngle, linLen) : states)
                            return curPos
getNext (AdjLen a)     = do ((curPos, curVec, adjAngle, linLen) : states) <- get
                            let newLen = linLen + a
                            put ((curPos, curVec, adjAngle, newLen) : states)
                            return curPos
getNext PushState      = do ((curPos, curVec, adjAngle, linLen) : states) <- get
                            put ((curPos, curVec, adjAngle, linLen) : (curPos, curVec, adjAngle, linLen) : states)
                            return curPos

getNext PopState       = do stateL <- get
                            case stateL of
                              (_ : (curPos, curVec, adjAngle, linLen) : states) ->
                                do put ((curPos, curVec, adjAngle, linLen) : states)
                                   return $ curPos >+ Point (-10000, -10000)
                              ((curPos, curVec, adjAngle, linLen) : states) ->
                                do put ((curPos, curVec, adjAngle, linLen) : states)
                                   return $ curPos


-- From Numeric.Limits, import isn't working

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
