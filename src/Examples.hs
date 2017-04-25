-- | This file containns examples of common fractals represented as LSystems
-- | It also gives the option of drawing any of these using Haskell's Gloss
-- | library

module Examples where
import LSystem
import Draw
import Prelude hiding (lookup)
import Test.QuickCheck
import Data.Monoid
import Graphics.Gloss
--------------------------------------------------------------------------------
-- | Examples

-- Sierpinski Fractal
sierpinski :: LSystem
sierpinski = LSystem "A-G-G" (r1 <> r2)
             (makeDefaultDrawRules (2 * pi / 3) <> dr1 <> dr2) where
  r1 = makeRule 'A' "A-G+A+G-A"
  r2 = makeRule 'G' "GG"
  dr1 = makeDrawRule 'A' Forward
  dr2 = makeDrawRule 'G' Forward

-- Sierpinski Arrowhead Fractal
sierpinskiArrowhead :: LSystem
sierpinskiArrowhead =
  LSystem "A" (r1 <> r2) (makeDefaultDrawRules (pi / 3) <> dr1 <> dr2) where
  r1 = makeRule 'A' "+B-A-B+"
  r2 = makeRule 'B' "-A+B+A-"
  dr1 = makeDrawRule 'A' Forward
  dr2 = makeDrawRule 'B' Forward

-- Dragon Fractal
dragon :: LSystem
dragon = LSystem "FX" (r1 <> r2) (makeDefaultDrawRules (pi / 2)) where
  r1 = makeRule 'X' "X+YF+"
  r2 = makeRule 'Y' "-FX-Y"

-- Hilbert Curve
hilbert :: LSystem
hilbert = LSystem "X" (r1 <> r2) (makeDefaultDrawRules (pi / 2)) where
  r1 = makeRule 'X' "-YF+XFX+FY-"
  r2 = makeRule 'Y' "+XF-YFY-FX+"

-- Gosper Curve
gosper :: LSystem
gosper = LSystem "A" (r1 <> r2)
         (makeDefaultDrawRules (pi / 3) <> dr1 <> dr2) where
  r1 = makeRule 'A' "A-B--B+A++AA+B-"
  r2 = makeRule 'B' "+A-BB--B-A++A+B"
  dr1 = makeDrawRule 'A' Forward
  dr2 = makeDrawRule 'B' Forward

-- Plant Curve
plant :: LSystem
plant = LSystem "X" (r1 <> r2) (makeDefaultDrawRules (25 * pi / 180)) where
  r1 = makeRule 'X' "F-[[X]+X]+F[+FX]-X"
  r2 = makeRule 'F' "FF"

--------------------------------------------------------------------------------
-- | Code for drawing curves

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

main :: IO ()
main = do
  lsys <- generate arbitrary
  display window background (drawPicture 6 lsys)
