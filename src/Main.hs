module Main where
import Graphics.Gloss
import LSystem
import Draw
import Test.QuickCheck
-- | Code for drawing curves

-- draws a Picture from an LSystem
drawPicture :: Int -> LSystem -> Picture
drawPicture depth lsys = pictures (line <$> getPaths depth lsys)

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

main :: IO ()
main = do
  lsys <- generate arbitrary
  display window background (drawPicture 6 lsys)
