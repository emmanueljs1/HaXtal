module Main where

import Draw
import LSystem
import Graphics.Gloss

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

drawingSierpinski :: Picture
drawingSierpinski = drawLSystem sierpinski 8

drawingDragon :: Picture
drawingDragon = drawLSystem dragon 15

drawingHilbert = drawLSystem hilbert 8

drawingGosper = drawLSystem gosper 4

drawingSierpinskiArrowhead = drawLSystem sierpinskiArrowhead 6

main :: IO ()
main = display window background drawingSierpinskiArrowhead
