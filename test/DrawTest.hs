-- | Code for testing Draw.hs
module DrawTest where

import Draw
import Test.QuickCheck

prop_singlePointBBox :: Point -> Bool
prop_singlePointBBox p = (x == minX b) && (y == minY b) &&
                         (x == maxX b) && (y == maxY b)
                         where
                           x = getX p
                           y = getY p
                           b = makeBoundingBox p

prop_drawBounds :: [Point] -> Property
prop_drawBounds ps =
  not (null ps) ==> (minx == minX b) && (miny == minY b) &&
                    (maxx == maxX b) && (maxy == maxY b)
                    where
                      xs   = map getX ps
                      ys   = map getY ps
                      minx = minimum xs
                      miny = minimum ys
                      maxx = maximum xs
                      maxy = maximum ys
                      b    = getDrawBounds ps

main :: IO ()
main = do quickCheck prop_singlePointBBox
          quickCheck prop_drawBounds