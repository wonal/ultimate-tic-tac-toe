module Board where 
import Data.List
import Data.List.Split
import Data.Char
import System.IO

size :: Int
size = 3

data Player = O | X | E
              deriving (Eq)

instance Show Player where 
    show O = "O"
    show X = "X"
    show E = " "

type InnerGame = [[Player]]
type OuterGame = [[(InnerGame, Player)]]

data ErrorStatus = None | BoundsError | OccupiedError | RuleError
                   deriving (Show, Eq)

data Status g e = Decided | InProgress g | Error e
                  deriving (Show, Eq)

type Position = Int

nextPlayer :: Player -> Player
nextPlayer O = X
nextPlayer X = O
nextPlayer E = E 

--For an InnerGame: emptyGame E, for an OuterGame: emptyGame (emptyGame E, E)
emptyGame :: a -> [[a]]
emptyGame a = replicate size (replicate size a)

fullGame :: InnerGame -> Bool 
fullGame g = length (filter (/= E) (concat g)) == size^2

checkCatsGame :: OuterGame -> Bool
checkCatsGame gs =  length (filter (==True) [(win g) /= E || catsGame g | g <- concat $ map (map fst) gs]) == size^2

catsGame :: InnerGame -> Bool
catsGame ig = fullGame ig && (win ig == E) 

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
                                           updateBoard grid value index = chunksOf 3 ((take index grid) ++ [(value, win value)] ++ (drop (index+1) grid))

move :: InnerGame -> Position -> Player -> Status InnerGame ErrorStatus
move ig pos p | ((win ig /= E) || catsGame ig)                            = Decided  
              | (checkPosition == None)                                   = InProgress board
              | otherwise                                                 = Error checkPosition
              where checkPosition = validPosition ig pos
                    rs = concat ig
                    board = chunksOf 3 ((take pos rs) ++ [p] ++ (drop (pos+1) rs))
                    
validPosition :: InnerGame -> Position -> ErrorStatus
validPosition ig pos | not inbounds = BoundsError
                     | inbounds && (not free) = OccupiedError
                     | otherwise = None
                     where inbounds = 0 <= pos && pos < (size^2)
                           free = (concat ig !! pos) == E
                     
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



