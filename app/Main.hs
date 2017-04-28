module Main where

import Graphics.Gloss
import Examples
import LSystem
import Draw
import Test.QuickCheck
import Text.Read
import Data.Maybe
import Control.Monad

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

samplePictures :: [Picture]
samplePictures = [
                   drawPicture 8 sierpinski,
                   drawPicture 10 dragon,
                   drawPicture 8 dragon2,
                   drawPicture 6 hilbert,
                   drawPicture 4 gosper,
                   drawPicture 6 sierpinskiArrowhead,
                   drawPicture 6 plant,
                   drawPicture 3 kochLake
                 ]

main :: IO ()
main = do
   putStrLn "Pick a Random LSystem Generator or a pregenerated LSystem"
   putStrLn "0: can use all constants"
   putStrLn "1: only turns"
   putStrLn "2: turns and pushes/pops"
   putStrLn "3: turns and line increases/decreases"
   putStrLn "4: turns and angle increases/decreases"
   putStrLn "5: Sierpinski"
   putStrLn "6: Dragon"
   putStrLn "7: Dragon with angle and line increases"
   putStrLn "8: Hilbert"
   putStrLn "9: Gosper"
   putStrLn "10: Sierpinski Arrowhead"
   putStrLn "11: Plant"
   putStrLn "12: Koch Lake"
   s <- getLine
   let op = fromMaybe 0 (readMaybe s)
   lsys <- generate (resize op arbitrary)
   let pic
         | op < 5 = drawPicture 6 lsys
         | op < 13 = samplePictures !! (op - 5)
         | otherwise = drawPicture 3 kochLake
   when (op < 5) $ putStrLn "Random LSystem:" >> print lsys
   display window background pic
