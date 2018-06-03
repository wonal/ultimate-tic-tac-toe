import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Color
import Board
import Data.List(elemIndex)

a::Float
a = 20

b :: Float
b = 60

e:: Float
e = 80

f::Float
f = 240

g::Float
g = 81

h::Float
h = 79

unit :: Float
unit = 160

outerGrid :: Picture
outerGrid = Pictures [Polygon [(-h,f),(-g,f),(-g,-f),(-h,-f)],
                       Polygon [(h,f),(g,f),(g,-f),(h,-f)],
                       Polygon [(f,h),(f,g),(-f,g),(-f,h)],
                       Polygon [(f,-h),(f,-g),(-f,-g),(-f,-h)]]
innerGrid :: Picture
innerGrid = Pictures [Line [(-a,b),(-a,-b)], Line [(a,b),(a,-b)], Line [(-b,a),(b,a)], Line [(-b,-a),(b,-a)]]

board :: Picture
board = Pictures [outerGrid, innerGrid, Translate (-unit) 0 innerGrid, Translate unit 0 innerGrid,
                  Translate (-unit) (-unit) innerGrid, Translate 0 (-unit) innerGrid, Translate unit (-unit) innerGrid,
                  Translate (-unit) unit innerGrid, Translate 0 (unit) innerGrid, Translate unit unit innerGrid]

game :: OuterGame -> Picture
game g = Pictures [board]


centerX :: Picture
centerX = Pictures [Scale (0.25::Float) (0.25::Float) (Translate (-35::Float) (-50::Float) (Text "X"))]

drawX :: Points -> Picture 
drawX ps = Pictures [Translate (fst x) (snd x) centerX]
                    where x = midpoint ps

drawO :: Points -> Picture
drawO ps = Pictures [Translate (fst x) (snd x) (Circle (12::Float))]
                    where x = midpoint ps

test :: Picture
test = Pictures [Circle (12::Float), innerGrid, Scale (0.25::Float) (0.25::Float) (Translate (-35::Float) (-50::Float) (Text "X")), drawX ((20::Float,20::Float),(60::Float,60::Float)),
                 drawO ((-20::Float,-60::Float),(20::Float,-20::Float))]

drawpic p = display (InWindow "board" (768,620) (0,0)) white (p)

--A pair of points to determine the bounds of a cell
type Points = ((Float, Float),(Float,Float))


cells :: [Points]
cells = [((-f,e),(-e,f)), ((-e,e),(e,f)), ((e,e),(f,f)), 
         ((-f,-e),(-e,e)), ((-e,-e),(e,e)), ((e,-e),(f,e)), 
         ((-f,-f),(-e,-e)), ((-e,-f),(e,-e)), ((e,-f),(f,-e))]

--Center inner game
centerGrid :: [Points]
centerGrid = [((-b,a),(-a,b)), ((-a,a),(a,b)), ((a,a),(b,b)), 
              ((-b,-a),(-a,a)), ((-a,-a),(a,a)), ((a,-a),(b,a)), 
              ((-b,-b),(-a,-a)), ((-a,-b),(a,-a)), ((a,-b),(b,-a))]

positions :: [[Points]]
positions = [trnslt unit (-unit) centerGrid, trnslt 0 unit centerGrid, trnslt unit unit centerGrid,
             trnslt (-unit) 0 centerGrid, centerGrid, trnslt unit 0 centerGrid,
             trnslt (-unit) (-unit) centerGrid, trnslt 0 (-unit) centerGrid, trnslt unit (-unit) centerGrid]


trnslt :: Float -> Float -> [Points] -> [Points]
trnslt x y ps = [(( (fst(fst pair))+x , (snd(fst pair))+y ), ((fst(snd pair))+x , (snd(snd pair))+y))| pair <- ps]

midpoint :: Points -> (Float, Float)
midpoint ps = (((fst(fst ps)) + (fst(snd ps)))/2 , (((snd(fst ps)) + (snd(snd ps)))/2))


getCell :: (Float, Float) -> [Points] -> [[Points]] -> Maybe (Int,Int)
getCell p c ps = case [pair | pair <- c, pointInBox p (fst pair) (snd pair)] of
                      [] -> Nothing
                      (x:xs) -> case elemIndex x c of
                                     Nothing -> Nothing
                                     Just i -> case [pos | pos <- (ps !! i), pointInBox p (fst pos) (snd pos) ] of
                                                    [] -> Nothing
                                                    (y:ys) -> case elemIndex y (ps !! i) of
                                                               Nothing -> Nothing
                                                               Just j -> Just (i,j)

{-}
playGame :: IO ()
playGame = do let window = InWindow "game window"  (768,512) (20,20)
              let game = emptyGame (emptyGame E,E)
              play window white 0 game render handleEvent (\_ y -> y)
                  
handleEvent :: Event -> OuterGame -> OuterGame
handleEvent (EventKey (MouseButton LeftButton) Down _ (x,y)) = "The x coordinate is: " ++ show x ++ " and the y coordinate is " ++ show y
-}
             














