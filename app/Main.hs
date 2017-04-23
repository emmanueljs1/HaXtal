module Main where

import Draw
import LSystem
import Graphics.Gloss
import Control.Monad.Trans.State.Lazy

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

drawingPlant = drawLSystem plant 6


main :: IO ()
-- main = putStrLn $ show (getAll [Forward, Forward, LeftTurn, PushState, Forward, RightTurn, Forward, PopState, RightTurn, RightTurn, Forward, Forward] [((0, 0), (1, 0))] )
-- main = putStrLn (show (runState (getNext Forward) [((0.0, 0.0), (1.0, 0.0))]))
main = display window background drawingPlant
