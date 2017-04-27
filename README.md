<h1> CIS 552 Final Project </h1>

<b>Members: </b>

David Buckman - dbuckman

Emmanuel Suarez - emsu

Kyle Rosenbluth - kyro

<h2>How To Run: </h2>

<h4>Desktop App: </h4>
stack build <br>
stack exec haxtal-exe

<h4>Testing Suite: </h4>
stack test

<h4>Web App: </h4>

git clone https://github.com/reflex-frp/reflex-platform

Then, navigate into the reflex-platform folder and run the try-reflex command.

cd reflex-platform
./try-reflex

Then, cd back to this folder in the nix shell, and run `ghcjs Web.hs`
Then go into the Web.jsexe folder, and double click index.html

<h2>Overview: </h2>

<i>Main.hs </i>

Create pictures from example LSystems, and present the user with a menu to display example fractals or choose parameters for a random fractal.

<i>LSystem.hs </i>

Definition of the LSystem data type. Includes show and arbitrary instance, and helper functions for defining rules to be used in user-generated LSystems. Also includes a definition of LSysComps, a string representation of an LSystem, along with its monoid instance and a function for converting LSysComps to LSystems.

<i>Draw.hs </i>

Module that allows for conversion from an LSystem definition to a format that can be displayed. This format is a list of lines, where each line is a list of points.

<i>Examples.hs </i>

Contains definitions of the example LSystems used in Main.hs.

<i>Spec.hs </i>

Top level test suite file.

<i>LSystemTest.hs </i>

Contains tests for LSystem.hs.

<i>DrawTest.hs </i>

Contains tests for Draw.hs.

<h2>Dependencies:</h2>
- Stack <br>
- GHCJS (Web only) <br>
- Reflex (Web only) <br>
- Gloss (App only) <br>
- QuickCheck <br>
- HUnit

<h2>Cool LSystems: </h2>

<i>+1</i><br>
L : SYS
S : F|+[F->Y[S]]
Y : [--F-F+FY]+

<i>VERY COOL FRACTAL, RANDOMLY GENERATED</i><br>
Start: Y
Variables: F,X,Y
Angle: 91.0
Rules:
F: YX
X: +[[-F
Y: F-+-+FYX]

<i>CHRISTMAS WREATH, RANDMOLY GENERATED</i><br>
Start: XX+
Variables: F,X,Y
Angle: 283.0
Rules:
F: FYX]-
X: YX[FYX
Y: XFY]YF[[]
