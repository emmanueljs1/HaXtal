module Main where

import Draw
import LSystem
import Graphics.Gloss

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

drawingSierpinski :: Picture
drawingSierpinski = drawLSystem sierpinski 6

drawingDragon :: Picture
drawingDragon = drawLSystem dragon 15

main :: IO ()
main = display window background drawingSierpinski
