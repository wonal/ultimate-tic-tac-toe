module Gui where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point
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
g = e + 1

h::Float
h = e - 1

unit :: Float
unit = 160

data Game = Game {
    pieces :: OuterGame,
    lastCell :: Maybe Int,
    currentPlayer :: Player, 
    message :: String,
    score :: [Player],
    finished :: Bool
} deriving (Show)

gameInit :: Game
gameInit = Game{
    pieces = emptyGame (emptyGame E, E),
    lastCell = Nothing,
    currentPlayer = X,
    message = "",
    score = concat $ map (map snd) (emptyGame (emptyGame E,E)),
    finished = False
}

outerGrid :: Picture
outerGrid = Pictures [Polygon [(-h,f),(-g,f),(-g,-f),(-h,-f)],
                       Polygon [(h,f),(g,f),(g,-f),(h,-f)],
                       Polygon [(f,h),(f,g),(-f,g),(-f,h)],
                       Polygon [(f,-h),(f,-g),(-f,-g),(-f,-h)]]

innerGrid :: Picture
innerGrid = Pictures [Line [(-a,b),(-a,-b)], Line [(a,b),(a,-b)], Line [(-b,a),(b,a)], Line [(-b,-a),(b,-a)]]

grid :: Picture
grid = Pictures [outerGrid, innerGrid, Translate (-unit) 0 innerGrid, Translate unit 0 innerGrid,
                  Translate (-unit) (-unit) innerGrid, Translate 0 (-unit) innerGrid, Translate unit (-unit) innerGrid,
                  Translate (-unit) unit innerGrid, Translate 0 (unit) innerGrid, Translate unit unit innerGrid]

markXO :: OuterGame -> Picture
markXO g = Pictures [if (fst x == X) then drawX (snd x) (1.0) else drawO (snd x) (1.0) (0.0) black | x <- filter (\(a,b) -> a /= E) (zip (concat $ concat $ map fst (concat g)) (concat $ positions)) ]

render :: Game -> Picture
render g = Pictures [grid, markXO (pieces g), turnString g, displayMessage (message g) g, displayNext (lastCell g),
                     displayWins (score g), Scale (0.22) (0.22) (Translate (-unit * 4.5) (unit * 7.75) (Text "Ultimate Tic-Tac-Toe!"))]

displayNext :: Maybe Int -> Picture
displayNext c = case c of
                     Nothing -> Blank
                     Just num -> drawO (cells !! (num)) (7.0) (2.0) blue

displayWins :: [Player] -> Picture
displayWins gs = Pictures [if (fst x == X) then drawX (snd x) (4.0) else drawO (snd x) (4.0) (0.0) black | x <- filter (\(a,b) -> a /= E) (zip gs cells)]

displayMessage :: String -> Game -> Picture
displayMessage m g = Scale (0.14) (0.14) (Translate (-unit * 6) (-unit * 12.20) (Color red (Text m)))

turnString :: Game -> Picture
turnString g = Scale (0.18) (0.18) (Translate (-unit * 12.5) (-unit * 9.5) (Text ("Player's turn: " ++ show (currentPlayer g))))

drawX :: Points -> Float -> Picture 
drawX ps scale = Pictures [Translate (fst x) (snd x) (centerX scale)]
                    where x = midpoint ps

centerX :: Float -> Picture
centerX s = Pictures [Scale (0.25 * s) (0.25 * s) (Translate (-35.0) (-50.0) (Text "X"))]

drawO :: Points -> Float -> Float -> Color -> Picture
drawO ps scale thick color = Pictures [Translate (fst x) (snd x) (Color color (ThickCircle (12.0 * scale) (thick)))] 
                    where x = midpoint ps

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
positions = [trnslt (-unit) unit centerGrid, trnslt 0 unit centerGrid, trnslt unit unit centerGrid,
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

playGame :: IO ()
playGame = do let window = InWindow "ultimate tic tac toe"  (768,620) (0,0)
              play window white 0 gameInit render handleEvent (\_ x -> x)
                 
              
handleEvent :: Event -> Game -> Game
handleEvent (EventKey (MouseButton LeftButton) Down _ (x,y)) g = 
            case finished g of 
                 True -> g {message = checkWin (pieces g)}
                 False -> case getCell (x,y) cells positions of
                               Nothing    -> g {message = "Error: Invalid position."} 
                               Just (i,j) -> case selectGame ((pieces g), c) i j (currentPlayer g) of 
                                                  Error BoundsError      -> g {message = "Error: Invalid position.  Choose a valid position."}
                                                  Error OccupiedError    -> g {message = "Error: Position is occupied."}
                                                  Error RuleError        -> g {message = "Error: Per the rules, you must move in cell " ++ (show (c+1))}
                                                  Decided                -> g {message = "Error: That game is already decided! Move again."} 
                                                  InProgress ip          -> case checkWin ip of 
                                                                                 "" -> g {pieces = ip, lastCell = Just j, currentPlayer = nextPlayer (currentPlayer g), score = concat $ map (map snd) ip, message = ""}  
                                                                                 _  -> g {pieces = ip, currentPlayer = E, message = checkWin ip, score = concat $ map (map snd) ip, finished = True}
                                    where c = case lastCell g of
                                                   Nothing -> i
                                                   Just val -> val
handleEvent _ g = g
             
checkWin :: OuterGame -> String
checkWin g    | turnResult == X = "Player X wins!\n"
              | turnResult == O = "Player O wins!\n"
              | checkCatsGame g = "It's a draw!\n"
              | otherwise       = ""
              where turnResult = winner g














