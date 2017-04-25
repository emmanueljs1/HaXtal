module Main where

import Draw
import LSystem
import Graphics.Gloss
import Examples

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

drawingSierpinski :: Picture
drawingSierpinski = drawPicture 12 sierpinski

drawingDragon :: Picture
drawingDragon = drawPicture 15 dragon

drawingHilbert = drawPicture 8 hilbert

drawingGosper = drawPicture 4 gosper

drawingSierpinskiArrowhead = drawPicture 6 sierpinskiArrowhead

drawingPlant = drawPicture 6 plant

drawArb = do curve <- arbCurve
             display window background (drawPicture 6 curve)


main :: IO ()
main = drawArb
-- main = display window background drawingSierpinski