<h1> CIS 552 Final Project </h1>

<b>Members: </b>

David Buckman

Emmanuel Suarez

Kyle Rosenbluth

Things to do for haxtal

- [x] Arbitrary (Emma)
- [ ] Animate the draw (Maybe) (Kyle)
- [ ] Zoom (for certain ones) (Just David)
- [ ] Tests and commenting (Emma but rly all of us)
- [ ] Refactor state push and pop (David)
- [ ] Increase angle, increase line size (David or Kyle)
- [ ] Styling (Kyle)
- [ ] Orient all fractals to be visible (Kyle and David)
- [ ] LSYstem input to instant generation (Emma)
- [ ] Random fractal button (Kyle and Emma)


L : SYS
S : F|+[F->Y[S]]
Y : [--F-F+FY]+

start = "Y+FY-XX-" rules = [('F',"F++F"),('X',"F"),('Y',"F+")]

Start: YY+X+X--XX
Variables: F,X,Y
Angle: 122.0
Rules:
 F: XY+X---+
 X: YXYXFX
 Y: X+YXF

Start: X-YXYX+-F
Variables: F,X,Y
Angle: 135.0
Rules:
  F: FYF-XX
  X: FY+Y
  Y: YF+FF

<b> twin circles </b>
  Start: XX+
  Variables: F,X,Y
  Angle: 251.0
  Rules:
   F: Y+--
   X: X-X
   Y: X+
