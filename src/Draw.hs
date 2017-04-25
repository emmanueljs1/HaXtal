module Draw where

import LSystem
import Control.Monad.Trans.State.Lazy
import Graphics.Gloss hiding (Vector)
import Debug.Trace

type Vector = (Float, Float)

-- adds two vectors
(>+) :: Vector -> Vector -> Vector
(a1, a2) >+ (b1, b2) = (a1 + b1, a2 + b2)

-- rotates a vector by a specified angle
rotateV :: Float -> Vector -> Vector
rotateV r (x, y)
 =      (  x * cos r - y * sin r
        ,  x * sin r + y * cos r)

-- draws a Picture from an LSystem
drawPicture :: Int -> LSystem -> Picture
drawPicture depth lsys = pictures (line <$> getPaths depth lsys)

-- Gets vector paths that represent visualization of an LSystem
getPaths :: Int -> LSystem -> [[Vector]]
getPaths depth lsys = removeBad $ start : rest where
  start = (0.0, 0.0)
  rest = getAll (expand lsys !! depth) initVector
  initVector = [((0.0, 0.0), (0.0, -1.0))]

--
getAll :: [Symbol] -> [(Vector, Vector)] -> [Vector]
getAll [] _        = []
getAll (s : ss) st = val : getAll ss nst
                     where
                       (val, nst) = runState (getNext s) st


-- can be used for rounding angles when 90deg,
-- e.g. mapTuple (fromIntegral . round) $ rotateV (pi / 2) curVec
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

--
getNextLine :: [Vector] -> ([Vector], [Vector])
getNextLine []            = ([], [])
getNextLine (p : ps)
              | fst p < -5000 =
                trace (show $ length ps) ([], mapTuple (10000 +) p : ps)
              | otherwise = (p : ln, remPts)
              where
                (ln, remPts) = getNextLine ps

--
removeBad :: [Vector] -> [[Vector]]
removeBad []  = []
removeBad pts = ln : removeBad remPts
                where
                  (ln, remPts) = getNextLine pts

--
getNext :: Symbol -> State [(Vector, Vector)] Vector
getNext Forward   = do ((curPos, curVec) : states) <- get
                       let newPos = curPos >+ curVec
                       put ((newPos, curVec) : states)
                       return newPos
getNext (Turn a)  = do ((curPos, curVec) : states) <- get
                       let newVec = rotateV a curVec
                       put ((curPos, newVec) : states)
                       return curPos
getNext PushState = do ((curPos, curVec) : states) <- get
                       put ((curPos, curVec) : (curPos, curVec) : states)
                       return curPos

getNext PopState  = do ((oldPos, oldVec) : (curPos, curVec) : states) <- get
                       put ((curPos, curVec) : states)
                       return $ mapTuple (-10000 +) curPos

--modify (\curStateL -> if length curStateL > 1
--                      then tail curStateL else curStateL)
