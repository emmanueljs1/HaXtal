module Main where

import Draw
import LSystem
import Graphics.Gloss

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = drawLSystem sierpinski 10

main :: IO ()
main = display window background drawing
