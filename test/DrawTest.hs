-- | Code for testing Draw.hs
module DrawTest where

import Draw
import LSystem
import Test.QuickCheck
import Test.HUnit
import Control.Monad.Trans.State.Lazy

prop_singlePointBBoxEqPoint :: Point -> Bool
prop_singlePointBBoxEqPoint p = (x == minX b) && (y == minY b) &&
                                (x == maxX b) && (y == maxY b)
                                where
                                  x = getX p
                                  y = getY p
                                  b = makeBoundingBox p

prop_drawBoundsEqMinMax :: [Point] -> Property
prop_drawBoundsEqMinMax ps =
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

roundAll :: [(DrawCommand, Point)] -> [(DrawCommand, Point)]
roundAll = map (\(d, p) -> (d, Point ((fromInteger . round) (getX p), (fromInteger . round) (getY p))))

testGetAll :: Test
testGetAll = TestList [
  getAll [] [(Point (0, 0), Direction (1, 0), 0, 1)] ~?= [],
  getAll [Forward] [(Point (0, 0), Direction (1, 0), 0, 1)] ~?= [(Pt, Point (1, 0))],
  roundAll (getAll [Forward, Turn (pi / 2), Forward] [(Point (0, 0), Direction (1, 0), 0, 1)]) ~?= [(Pt, Point (1, 0)), (NOP, Point (1, 0)), (Pt, Point (1, 1))],
  roundAll (getAll [Forward, PushState, Turn (pi / 2), Forward, PopState, Forward] [(Point (0, 0), Direction (1, 0), 0, 1)]) ~?= [(Pt, Point (1, 0)), (NOP, Point (1, 0)), (NOP, Point (1, 0)), (Pt, Point (1, 1)), (NewLine, Point (1, 0)), (Pt, Point (2, 0))],
  roundAll (getAll [Forward, Jump, Forward] [(Point (0, 0), Direction (1, 0), 0, 1)]) ~?= [(Pt, Point (1, 0)), (NewLine, Point (2, 0)), (Pt, Point (3, 0))],
  roundAll (getAll [Forward, AdjAngle (pi / 2), Turn 0, Forward] [(Point (0, 0), Direction (1, 0), 0, 1)]) ~?= [(Pt, Point (1, 0)), (NOP, Point (1, 0)), (NOP, Point (1, 0)), (Pt, Point (1, 1))],
  roundAll (getAll [Forward, AdjLen 1, Forward] [(Point (0, 0), Direction (1, 0), 0, 1)]) ~?= [(Pt, Point (1, 0)), (NOP, Point (1, 0)), (Pt, Point (3, 0))] ]

testGetNextLine :: Test
testGetNextLine = TestList [
  getNextLine [] ~?= ([], []),
  getNextLine [(Pt, Point (1, 1))] ~?= ([Point (1, 1)], []),
  getNextLine [(Pt, Point (1, 1)), (Pt, Point (2, 2))] ~?= ([Point (1, 1), Point (2, 2)], []),
  getNextLine [(Pt, Point (1, 1)), (NewLine, Point (2, 2))] ~?= ([Point (1, 1)], [(Pt, Point (2, 2))]),
  getNextLine [(NOP, Point (1, 1)), (NewLine, Point (2, 2))] ~?= ([], [(Pt, Point (2, 2))]) ]

testSplitLines :: Test
testSplitLines = TestList [
  splitLines [(Pt, Point (1, 1)), (Pt, Point (2, 2))] ~?= [[Point (1, 1), Point (2, 2)]],
  splitLines [(Pt, Point (1, 1)), (NOP, Point (2, 2))] ~?= [[Point (1, 1)]],
  splitLines [(Pt, Point (1, 1)), (NewLine, Point (2, 2))] ~?= [[Point (1, 1)], [Point (2, 2)]],
  splitLines [(Pt, Point (1, 1)), (NOP, Point (0, 0)), (NewLine, Point (2, 2)),
              (NOP, Point (0, 0)), (Pt, Point (3, 3))] ~?= [[Point (1, 1)], [Point (2, 2), Point (3, 3)]],
  splitLines [(NOP, Point (1, 1)), (NOP, Point (2, 2))] ~?= [[]] ]

prop_getNextDrawOp :: Symbol -> [(Point, Direction, Float, Float)] -> Property
prop_getNextDrawOp s st =
  not (null st) ==> fst (evalState (getNext s) st) ==
                      case s of
                        Forward    -> Pt
                        Jump       -> NewLine
                        Turn _     -> NOP
                        AdjAngle _ -> NOP
                        AdjLen _   -> NOP
                        PushState  -> NOP
                        PopState   -> if length st > 1 then NewLine else NOP

main :: IO ()
main = do quickCheck prop_singlePointBBoxEqPoint
          quickCheck prop_drawBoundsEqMinMax
          runTestTT testGetAll
          runTestTT testGetNextLine
          runTestTT testSplitLines
          quickCheck prop_getNextDrawOp
          return ()