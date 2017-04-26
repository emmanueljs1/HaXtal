module Draw where

import LSystem
import Control.Monad.Trans.State.Lazy
import Graphics.Gloss
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

getX :: Point -> Float
getX = fst

getY :: Point -> Float
getY = snd

-- adds two vectors
(>+) :: Vector -> Vector -> Vector
(a1, a2) >+ (b1, b2) = (a1 + b1, a2 + b2)

-- rotates a vector by a specified angle (code borrowed from Gloss package)
rotateV :: Float -> Vector -> Vector
rotateV r (x, y)
 =      (  x * cos r - y * sin r
        ,  x * sin r + y * cos r)

getDrawBounds :: [Point] -> BoundingBox
getDrawBounds = foldr ((<>) . makeBoundingBox) mempty

-- Gets vector paths that represent visualization of an LSystem
getPaths :: Int -> LSystem -> [[Point]]
getPaths depth lsys = removeBad $ start : rest where
  start = (0.0, 0.0)
  rest = getAll (expand lsys !! depth) initVector
  initVector = [((0.0, 0.0), (0.0, -1.0), 0)]

--
getAll :: [Symbol] -> [(Point, Vector, Float)] -> [Point]
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
              | fst p < -5000 = ([], mapTuple (10000 +) p : ps)
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
getNext :: Symbol -> State [(Point, Vector, Float)] Point
getNext Forward        = do ((curPos, curVec, adjAngle) : states) <- get
                            let newPos = curPos >+ curVec
                            put ((newPos, curVec, adjAngle) : states)
                            return newPos
getNext (Turn a)       = do ((curPos, curVec, adjAngle) : states) <- get
                            let newVec = rotateV (a + adjAngle) curVec
                            put ((curPos, newVec, adjAngle) : states)
                            return curPos
getNext (AdjAngle a)   = do ((curPos, curVec, adjAngle) : states) <- get
                            let newAngle = adjAngle + a
                            put ((curPos, curVec, newAngle) : states)
                            return curPos
getNext PushState      = do ((curPos, curVec, adjAngle) : states) <- get
                            put ((curPos, curVec, adjAngle) : (curPos, curVec, adjAngle) : states)
                            return curPos

getNext PopState       = do (_ : (curPos, curVec, adjAngle) : states) <- get
                            put ((curPos, curVec, adjAngle) : states)
                            return $ mapTuple (-10000 +) curPos

--modify (\curStateL -> if length curStateL > 1
--                      then tail curStateL else curStateL)


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
