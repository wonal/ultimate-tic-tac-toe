--Allison Wong
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

f::Float
f = 240

g::Float
g = 81

h::Float
h = 79 

unit :: Float
unit = 160

--A pair of corner points to determine the bounds of a cell
type Points = ((Float, Float),(Float,Float))

data Game = Game {
    moves :: OuterGame,     --holds the state of the game 
    lastCell :: Maybe Int,  --the position the last player played in
    currentPlayer :: Player, 
    message :: String,      --error or win message
    xoWins :: [Player],     --list of cell wins
    finished :: Bool        --if a winner has been found
} deriving (Show)

gameInit :: Game
gameInit = Game{
    moves = emptyGame (emptyGame E, E),
    lastCell = Nothing,
    currentPlayer = X,
    message = "",
    xoWins = concat $ map (map snd) (emptyGame (emptyGame E,E)),
    finished = False
}

render :: Game -> Picture
render g = Pictures [fullGrid, playerPieces (moves g), playerString (currentPlayer g), gameMessage (message g), visualCue (lastCell g) (moves g),
                     outerWins (xoWins g), Scale (0.22) (0.22) (Translate (-unit * 4.5) (unit * 7.75) (Text "Ultimate Tic-Tac-Toe!"))]

fullGrid :: Picture
fullGrid = Pictures [outerGrid, Pictures [Translate x y innerGrid | x <- [-unit, 0, unit], y <- [-unit, 0, unit]]]

outerGrid :: Picture
outerGrid = Pictures [Polygon [(-h,f),(-g,f),(-g,-f),(-h,-f)], Polygon [(h,f),(g,f),(g,-f),(h,-f)],
                       Polygon [(f,h),(f,g),(-f,g),(-f,h)], Polygon [(f,-h),(f,-g),(-f,-g),(-f,-h)]]

innerGrid :: Picture
innerGrid = Pictures [Line [(-a,b),(-a,-b)], Line [(a,b),(a,-b)], Line [(-b,a),(b,a)], Line [(-b,-a),(b,-a)]]

--The state of the board represented by only X's and O's.  Pair the flattened list of player pieces with their respective
--positions to determine where to draw.
playerPieces :: OuterGame -> Picture
playerPieces g = Pictures [if (fst x == X) then drawX (snd x) (1.0) else drawO (snd x) (1.0) (0.0) black | 
                    x <- filter (\(a,b) -> a /= E) (zip (concat $ concat $ map fst (concat g)) (concat $ positions)) ]

--Draws an X at the midpoint between two points
drawX :: Points -> Float -> Picture 
drawX ps scale = Pictures [Translate (fst x) (snd x) (centerX scale)]
                    where x = midpoint ps

centerX :: Float -> Picture
centerX s = Pictures [Scale (0.25 * s) (0.25 * s) (Translate (-35.0) (-50.0) (Text "X"))]

--Draws an O at the midpoint between two points and takes a scale factor, thickness, and color
drawO :: Points -> Float -> Float -> Color -> Picture
drawO ps scale thick color = Pictures [Translate (fst x) (snd x) (Color color (ThickCircle (12.0 * scale) (thick)))] 
                    where x = midpoint ps

playerString :: Player -> Picture
playerString p = Scale (0.18) (0.18) (Translate (-unit * 12.5) (-unit * 9.5) (Text ("Player's turn: " ++ show p)))

gameMessage :: String -> Picture
gameMessage m = Scale (0.14) (0.14) (Translate (-unit * 6) (-unit * 12.20) (Color red (Text m)))

--Displays a visual cue as to which cell the player needs to play next.
visualCue :: Maybe Int -> OuterGame -> Picture
visualCue c gs = case c of
                     Nothing -> Blank
                     Just num -> if (win games /= E || catsGame games) then Blank
                                                                       else drawO (outerGamePts !! (num)) (7.0) (2.0) blue
                                    where games = (concat $ map (map fst) (gs)) !! num

--Displays the status of an outer cell: if an X has won, etc. 
outerWins :: [Player] -> Picture
outerWins gs = Pictures [if (fst x == X) then drawX (snd x) (4.0) else drawO (snd x) (4.0) (0.0) black | 
                          x <- filter (\(a,b) -> a /= E) (zip gs outerGamePts)]


--Given a point within the screen application, returns either Nothing if the point is not within the valid board, and otherwise 
--returns the cell number and position number
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

--Handles a left mouse button action: checks that the move is valid and creates a new game with updated values (the player's move or an error message, etc.)
handleEvent :: Event -> Game -> Game
handleEvent (EventKey (MouseButton LeftButton) Down _ (x,y)) g = 
            case finished g of 
                 True -> g {message = checkWin (moves g)}
                 False -> case getCell (x,y) outerGamePts positions of
                               Nothing    -> g {message = "Error: Invalid position."} 
                               Just (i,j) -> case selectGame ((moves g), c) i j (currentPlayer g) of 
                                                  Error BoundsError      -> g {message = "Error: Invalid position.  Choose a valid position."}
                                                  Error OccupiedError    -> g {message = "Error: Position is occupied."}
                                                  Error RuleError        -> g {message = "Error: Per the rules, you must move in cell " ++ (show (c+1))}
                                                  Decided                -> g {message = "Error: That game is already decided! Move again."} 
                                                  InProgress ip          -> case checkWin ip of 
                                                                                 "" -> g {moves = ip, lastCell = Just j, currentPlayer = nextPlayer (currentPlayer g), xoWins = concat $ map (map snd) ip, message = ""}  
                                                                                 _  -> g {moves = ip, currentPlayer = E, message = checkWin ip, xoWins = concat $ map (map snd) ip, finished = True}
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

playGame :: IO ()
playGame = do let window = InWindow "ultimate tic tac toe"  (768,620) (0,0)
              play window white 0 gameInit render handleEvent (\_ x -> x)


--Corner points of the center inner game
centerGamePts :: [Points]
centerGamePts = [((-b,a),(-a,b)), ((-a,a),(a,b)), ((a,a),(b,b)), ((-b,-a),(-a,a)), ((-a,-a),(a,a)), ((a,-a),(b,a)), 
              ((-b,-b),(-a,-a)), ((-a,-b),(a,-a)), ((a,-b),(b,-a))]

--Corner points of the outer cells
outerGamePts :: [Points]
outerGamePts = [(((fst(fst ps))*(4.0),(snd(fst ps))*(4.0)),((fst(snd ps))*(4.0),(snd(snd ps))*(4.0))) | ps <- centerGamePts]

--Corner points of each position of the inner games
positions :: [[Points]]
positions = [translatePair (-unit) unit centerGamePts, translatePair 0 unit centerGamePts, translatePair unit unit centerGamePts,
             translatePair (-unit) 0 centerGamePts, centerGamePts, translatePair unit 0 centerGamePts,
             translatePair (-unit) (-unit) centerGamePts, translatePair 0 (-unit) centerGamePts, translatePair unit (-unit) centerGamePts]

translatePair :: Float -> Float -> [Points] -> [Points]
translatePair x y ps = [(( (fst(fst pair))+x , (snd(fst pair))+y ), ((fst(snd pair))+x , (snd(snd pair))+y))| pair <- ps]

midpoint :: Points -> (Float, Float)
midpoint ps = (((fst(fst ps)) + (fst(snd ps)))/2 , (((snd(fst ps)) + (snd(snd ps)))/2))















