--Allison Wong
module Board where 
import Data.List
import Data.Char
import System.IO

size :: Int
size = 3

--A player is represented as X, O, or E / empty
data Player = O | X | E
              deriving (Eq)

instance Show Player where 
    show O = "O"
    show X = "X"
    show E = " "

type InnerGame = [[Player]]               --An inner game of tic-tac-toe
type OuterGame = [[(InnerGame, Player)]]  --The outer game of tic-tac-toc, holding pairs of inner games and the respective winners

data ErrorStatus = None | BoundsError | OccupiedError | RuleError
                   deriving (Show, Eq)

data Status game err = Decided | InProgress game | Error err
                  deriving (Show, Eq)

nextPlayer :: Player -> Player
nextPlayer O = X
nextPlayer X = O
nextPlayer E = E 

emptyGame :: a -> [[a]]
emptyGame a = replicate size (replicate size a)

winner :: OuterGame -> Player
winner = win . map (map snd) 

win :: InnerGame -> Player
win ig = if null winList then E 
                         else head winList 
            where winList = filter (/=E) [columnWin ig, rowWin ig, diagonalWin ig]

rowWin :: InnerGame -> Player
rowWin rss = if null rowMatch then E
                              else head (head rowMatch)
                     where rowMatch = filter (\x -> x == [X,X,X] || x == [O,O,O]) rss

columnWin :: InnerGame -> Player
columnWin = rowWin . transpose 

diagonalWin :: InnerGame -> Player
diagonalWin g = if ((((g !! 0) !! 0) == middle) && (middle == ((g !! 2) !! 2))) ||
                   ((((g !! 0) !! 2) == middle) && (middle == (g !! 2) !! 0)) then middle 
                                                                               else E
                       where middle = (g !! 1) !! 1

fullGame :: InnerGame -> Bool 
fullGame g = length (filter (/= E) (concat g)) == size^2

catsGame :: InnerGame -> Bool
catsGame ig = fullGame ig && (win ig == E) 

--Only being called after checked for X or O win
checkCatsGame :: OuterGame -> Bool
checkCatsGame gs =  length (filter (==True) [(win g) /= E || catsGame g | g <- concat $ map (map fst) gs]) == size^2

type Position = Int
                    
validPosition :: InnerGame -> Position -> ErrorStatus
validPosition ig pos | not inbounds = BoundsError
                     | inbounds && (not free) = OccupiedError
                     | otherwise = None
                     where inbounds = 0 <= pos && pos < (size^2)
                           free = (concat ig !! pos) == E

--Attempts to make a move within an inner game
move :: InnerGame -> Position -> Player -> Status InnerGame ErrorStatus
move ig pos p | ((win ig /= E) || catsGame ig)                            = Decided  
              | (checkPosition == None)                                   = InProgress board
              | otherwise                                                 = Error checkPosition
              where checkPosition = validPosition ig pos
                    rs = concat ig
                    board = chop3 ((take pos rs) ++ [p] ++ (drop (pos+1) rs))
                    
--Given a game and a cell where the player must play next (mustPlay), checks that player's input (cell, pos) is valid (within the bounds of the board
-- and is within the rules: not attempting to place their X/O on a decided game, etc.) and creates a new game with updated values if so.  
selectGame :: (OuterGame, Int) -> Int -> Position -> Player -> Status OuterGame ErrorStatus
selectGame (g, mustPlay) cell pos p  | cell < 0 || cell >= size^2             = Error BoundsError
                                     | otherwise = case mustPlay /= cell of 
                                                        True -> case move (fst (games !! mustPlay)) pos p of
                                                                     Decided -> case move inner pos p of 
                                                                                     InProgress moveResult -> InProgress (updateBoard games moveResult cell)
                                                                                     Decided               -> Decided
                                                                                     Error e               -> Error e
                                                                     _       -> Error RuleError 
                                                        False -> case move inner pos p of
                                                                      Error e -> Error e
                                                                      Decided -> Decided
                                                                      InProgress normalMove -> InProgress (updateBoard games normalMove cell)
                                     where games = concat g
                                           inner = fst (games !! cell)
                                           updateBoard grid value index = chop3 ((take index grid) ++ [(value, win value)] ++ (drop (index+1) grid))

chop3 :: [a] -> [[a]]
chop3 xs = takeWhile (not.null) [(take 3) (drop (3 * n) xs) | n <- [0..len]]
                                where len = (length xs) `div` 3




