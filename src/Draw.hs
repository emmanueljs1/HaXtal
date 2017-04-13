module Draw where

import LSystem
import Graphics.Gloss
import Graphics.Gloss.Data.Vector

drawLSystem :: LSystem -> Int -> Picture
drawLSystem lsys depth = line $ d (0.0, 0.0) (0.0, 1.0) (expand lsys depth)
  where
    d _ _           []       = []
    d curPos curVec (o : os) = case op o of
      Forward  -> (curPos + curVec) : (d (curPos + curVec) curVec os)
      Turn deg -> (newP + newV) : (d newP newV os)
        where
          newV = (rotateV (deg * (pi / 180.0)) curVec)
          newP = curPos + curVec
