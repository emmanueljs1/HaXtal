module Examples where
import LSystem
import Data.Monoid

sierpinski :: LSystem
sierpinski = LSystem "A-G-G" (r1 <> r2)
             (makeDefaultDrawRules (2 * pi / 3) <> dr1 <> dr2) where
  r1 = makeRule 'A' "A-G+A+G-A"
  r2 = makeRule 'G' "GG"
  dr1 = makeDrawRule 'A' Forward
  dr2 = makeDrawRule 'G' Forward

sierpinskiArrowhead :: LSystem
sierpinskiArrowhead =
  LSystem "A" (r1 <> r2) (makeDefaultDrawRules (pi / 3) <> dr1 <> dr2) where
  r1 = makeRule 'A' "+B-A-B+"
  r2 = makeRule 'B' "-A+B+A-"
  dr1 = makeDrawRule 'A' Forward
  dr2 = makeDrawRule 'B' Forward

dragon :: LSystem
dragon = LSystem "FX" (r1 <> r2) (makeDefaultDrawRules (pi / 2)) where
  r1 = makeRule 'X' "X+YF+"
  r2 = makeRule 'Y' "-FX-Y"

hilbert :: LSystem
hilbert = LSystem "X" (r1 <> r2) (makeDefaultDrawRules (pi / 2)) where
  r1 = makeRule 'X' "-YF+XFX+FY-"
  r2 = makeRule 'Y' "+XF-YFY-FX+"

gosper :: LSystem
gosper = LSystem "A" (r1 <> r2)
         (makeDefaultDrawRules (pi / 3) <> dr1 <> dr2) where
  r1 = makeRule 'A' "A-B--B+A++AA+B-"
  r2 = makeRule 'B' "+A-BB--B-A++A+B"
  dr1 = makeDrawRule 'A' Forward
  dr2 = makeDrawRule 'B' Forward

plant :: LSystem
plant = LSystem "X" (r1 <> r2) (makeDefaultDrawRules (25 * pi / 180)) where
  r1 = makeRule 'X' "F-[[X]+X]+F[+FX]-X"
  r2 = makeRule 'F' "FF"
