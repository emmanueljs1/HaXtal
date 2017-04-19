module Main where

import Draw
import LSystem
import Graphics.Gloss

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

drawingSierpinski :: Picture
drawingSierpinski = drawLSystem sierpinski 5

drawingDragon :: Picture
drawingDragon = drawLSystem dragon 10

drawingHilbert = drawLSystem hilbert 5

drawingGosper = drawLSystem gosper 4

main :: IO ()
main = do 
       -- display window background drawingSierpinski
       display window background drawingGosper
