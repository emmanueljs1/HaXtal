module Draw where

import LSystem
import Graphics.Gloss
import Graphics.Gloss.Data.Vector

drawLSystem :: LSystem -> Int -> Picture
drawLSystem lsys depth = line $ (0.0, 0.0) : d (0.0, 0.0) (1.0, 0.0)
                         (expand lsys !! depth)
  where
    d _ _           []       = []
    d curPos curVec (o : os) = case o of
      -- Var _     -> d curPos curVec os
      Forward  -> (curPos + curVec) : d (curPos + curVec) curVec os
      RightTurn -> let newV = normalizeV (rotateV ((realToFrac (angle lsys)) * (pi / 180.0)) curVec) in
        (d curPos newV os)
      LeftTurn -> let newV = normalizeV (rotateV ((realToFrac (360 - (angle lsys))) * (pi / 180.0)) curVec) in
        (d curPos newV os)