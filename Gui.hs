import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Color
import Board

a :: Float
a = 0

b :: Float
b = 250

c :: Float
c = 500

d :: Float
d = 750

e::Float
e = 20

f::Float
f = 70

g::Float
g = 90

h::Float
h = 160

i::Float
i = 230

unit :: Float
unit = 125

outerGrid :: Picture
outerGrid = Pictures [(Line [(a,c),(d,c)]), 
                      (Line [(a,b),(d,b)]), 
                      --(Line [(b,a),(b,d)]), 
                      outerGrid1,
                      (Line [(c,a),(c,d)])]


outerGrid1 :: Picture
outerGrid1 = Pictures [Polygon [(249.0,0.0),(249.0,750.0),(251.0, 750.0),(251.0,0.0)],
                       Polygon [(499.0,0.0),(499.0,750.0),(501.0, 750.0),(501.0,0.0)],
                       Polygon [(0.0,499.0),(0.0,501.0),(750.0, 501.0),(750.0,499.0)],
                       Polygon [(0.0,249.0),(0.0,251.0),(750.0, 251.0),(750.0,249.0)]]

innerGrid :: Picture
innerGrid = Translate (-125::Float) (-125::Float) (Pictures [Line [(e,h),(i,h)], Line[(e,g),(i,g)], Line[(g,e),(g,i)], Line[(h,e),(h,i)]])

render :: Picture
render = Translate (-375::Float) (-375::Float) (Pictures [outerGrid1, Translate (unit) (unit) innerGrid, Translate (unit) (unit * 3) innerGrid,
                                                          Translate (unit) (unit * 5) innerGrid, Translate (unit * 3) (unit * 3) innerGrid,
                                                          Translate (unit * 3) (unit) innerGrid, Translate (unit * 3) (unit * 5) innerGrid,
                                                          Translate (unit * 5) (unit) innerGrid, Translate (unit * 5) (unit * 3) innerGrid,
                                                          Translate (unit * 5) (unit * 5) innerGrid])

drawpic p = display (InWindow "board" (1000,1000) (0,0)) white (p)

{-}
playGame :: IO ()
playGame = do let window = InWindow "game window"  (768,512) (20,20)
              let game = emptyGame (emptyGame E,E)
              play window white 0 game render handleEvent (\_ y -> y)
                  
handleEvent :: Event -> OuterGame -> OuterGame
handleEvent (EventKey (MouseButton LeftButton) Down _ (x,y)) = "The x coordinate is: " ++ show x ++ " and the y coordinate is " ++ show y
-}
             














