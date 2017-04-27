<h1> CIS 552 Final Project </h1>

<b>Members: </b>

David Buckman - dbuckman

Emmanuel Suarez - emsu

Kyle Rosenbluth - kyro

<h2>How To Run: </h2>

stack build <br>
stack exec haxtal-exe

stack test

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




<h2>Cool LSystems: </h2>

<i>+1</i>

L : SYS
S : F|+[F->Y[S]]
Y : [--F-F+FY]+

<i>VERY COOL FRACTAL, RANDOMLY GENERATED</i>

Start: Y
Variables: F,X,Y
Angle: 91.0
Rules:
F: YX
X: +[[-F
Y: F-+-+FYX]

<i>CHRISTMAS WREATH, RANDMOLY GENERATED</i>

Start: XX+
Variables: F,X,Y
Angle: 283.0
Rules:
F: FYX]-
X: YX[FYX
Y: XFY]YF[[]
