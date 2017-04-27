module Main where

import Graphics.Gloss
import Examples
import LSystem
import Draw
import Test.QuickCheck
import Text.Read
import Data.Maybe

-- | Code for drawing curves
toGlossPoint :: Draw.Point -> Graphics.Gloss.Point
toGlossPoint p = (getX p, getY p)

-- draws a Picture from an LSystem
drawPicture :: Int -> LSystem -> Picture
drawPicture depth lsys =
  pictures (line . (toGlossPoint <$>) <$> getPaths depth lsys)

window :: Display
window = InWindow "LSystems!" (200, 200) (10, 10)

background :: Color
background = white

drawingSierpinski :: Picture
drawingSierpinski = drawPicture 8 sierpinski

drawingDragon :: Picture
drawingDragon = drawPicture 10 dragon

drawingDragon2 :: Picture
drawingDragon2 = drawPicture 8 dragon2

drawingHilbert :: Picture
drawingHilbert = drawPicture 6 hilbert

drawingGosper :: Picture
drawingGosper = drawPicture 4 gosper

drawingSierpinskiArrowhead :: Picture
drawingSierpinskiArrowhead = drawPicture 6 sierpinskiArrowhead

drawingPlant :: Picture
drawingPlant = drawPicture 6 plant

drawingSunflower :: Picture
drawingSunflower = drawPicture 6 sunflower

drawingKochLake :: Picture
drawingKochLake = drawPicture 3 kochLake

main :: IO ()
main = do
   putStrLn "Pick a Random LSystem Generator or a pregenerated LSystem"
   putStrLn "0: can use all constants"
   putStrLn "1: only turns"
   putStrLn "2: turns and pushes/pops"
   putStrLn "3: turns and angle increases/decreases"
   s <- getLine
   let op = fromMaybe 0 (readMaybe s)
   lsys <- generate (resize op arbitrary)
   let pic =
     if op < 4 then
       drawPicture 6 lsys
     else
       drawingSierpinski
   display window background pic
