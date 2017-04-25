module Main where

import Graphics.Gloss
import Examples
import LSystem
import Draw
import Test.QuickCheck

-- | Code for drawing curves

-- draws a Picture from an LSystem
drawPicture :: Int -> LSystem -> Picture
drawPicture depth lsys = pictures (line <$> getPaths depth lsys)

window :: Display
window = InWindow "LSystems!" (200, 200) (10, 10)

background :: Color
background = white

drawingSierpinski :: Picture
drawingSierpinski = drawPicture 8 sierpinski

drawingDragon :: Picture
drawingDragon = drawPicture 15 dragon

drawingHilbert :: Picture
drawingHilbert = drawPicture 8 hilbert

drawingGosper :: Picture
drawingGosper = drawPicture 4 gosper

drawingSierpinskiArrowhead :: Picture
drawingSierpinskiArrowhead = drawPicture 6 sierpinskiArrowhead

drawingPlant :: Picture
drawingPlant = drawPicture 6 plant

drawingSunflower :: Picture
drawingSunflower = drawPicture 6 sunflower

main :: IO ()
main = do
  lsys <- generate arbitrary
  print lsys
  display window background (drawPicture 6 lsys)
