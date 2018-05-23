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
    show E = ""

type InnerGame = [[Player]]
type OuterGame = [[(InnerGame, Player)]]

data Status game = Error | Decided | InProgress game

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
checkCatsGame = catsGame . map (map snd)

catsGame :: InnerGame -> Bool
catsGame ig = fullGame ig && (win ig == E) 

{-}
selectGame :: (OuterGame, Int) -> Int -> Position -> Player -> (Status OuterGame, Int)
selectGame (g, mustPlay) cell pos p  | cell < 0 || cell >= size^2             = (Error, mustPlay)  --if invalid gamecell  incorporate with otherwise
                                     | otherwise = case move inner pos p of 
                                                        Error -> (Error, mustPlay)
                                                        Decided -> (Decided, pos)
                                                        InProgress moveResult -> (InProgress (chunksOf 3 ((take cell games) ++ [(moveResult, win moveResult)] ++ (drop (cell+1) games))), pos)
                                     where games = concat g
                                           inner = fst (games !! cell)
                                           -}
                                           
selectGame :: (OuterGame, Int) -> Int -> Position -> Player -> (Status OuterGame, Int)
selectGame (g, mustPlay) cell pos p  | cell < 0 || cell >= size^2             = (Error, mustPlay)  --if invalid gamecell  incorporate with otherwise
                                     | otherwise = case mustPlay /= cell of 
                                                        True -> case move inner mustPlay p of
                                                                     Decided -> case move inner pos p of 
                                                                                     InProgress moveResult -> (InProgress (updateBoard games moveResult cell), pos)
                                                                                     _                     -> (Error, mustPlay)
                                                                     _       -> (Error, mustPlay)
                                                        False -> case move inner pos p of
                                                                      Error -> (Error, mustPlay)
                                                                      Decided -> (Decided, pos)
                                                                      InProgress normalMove -> (InProgress (updateBoard games normalMove cell), pos)
                                     where games = concat g
                                           inner = fst (games !! cell)
                                           updateBoard grid value index = chunksOf 3 ((take index grid) ++ [(value, win value)] ++ (drop (index+1) grid))

move :: InnerGame -> Position -> Player -> Status InnerGame 
move ig pos p | valid && ((win ig /= E) || catsGame ig) = Decided  -- valid position but X or B won OR it's a draw --> not an error but shouldn't place on the board since it's already decided
              | valid                                   = InProgress board
              | otherwise                               = Error 
              where valid = validPosition ig pos
                    rs = concat ig
                    board = chunksOf 3 ((take pos rs) ++ [p] ++ (drop (pos+1) rs))

validPosition :: InnerGame -> Position -> Bool
validPosition ig pos = 0 <= pos && pos < (size^2) && (concat ig !! pos == E)

updatedBoard :: [a] -> Int -> a -> [[a]]
updatedBoard rs index update = chunksOf 3 ((take index rs) ++ [update] ++ (drop (index+1) rs))

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


----------------------------------------
-- IO

ultimatetictactoe :: IO ()
ultimatetictactoe = takeTurn (emptyGame (emptyGame E, E)) O Nothing

{-}
start :: OuterGame -> Player -> IO ()
start g p = do displayGame g
               cell <- getNum (prompt p ", enter the game number you wish to play in: ")
               pos <- getNum (prompt p ", enter the cell number you wish to play in: ")
               case fst (selectGame (g, cell) cell pos p) of 
                        Error                  -> do putStrLn "Error: invalid move"
                                                     start g p
                        Decided                -> do putStrLn "That game cell has already been decided!  You can play in any free game now."
                                                     start g p
                        InProgress updatedGame -> checkWin updatedGame p pos
            -}

checkWin :: OuterGame -> Player -> Int -> IO ()
checkWin g p c  | turnResult == X = putStrLn "Player X wins!\n"
                | turnResult == O = putStrLn "Player O wins!\n"
                | checkCatsGame g   = putStrLn "It's a draw!\n"
                | otherwise       =
                    takeTurn g (nextPlayer p)  (Just c)
                where turnResult = winner g


takeTurn :: OuterGame -> Player -> Maybe Int -> IO ()
takeTurn g p c =  do displayGame g
                     cell <- getNum (prompt p ", enter the game number you wish to play in: ")
                     pos <- getNum (prompt p ", enter the cell number you wish to play in: ")
                     let c' = case c of 
                                   Nothing -> cell
                                   Just a  -> a
                     case fst (selectGame (g, c') cell pos p) of 
                          Error                  -> do putStrLn "Error: invalid move"
                                                       takeTurn g p c
                          Decided                -> do putStrLn "That game cell has already been decided!  You can play in any free game now."
                                                       takeTurn g p c
                          InProgress updatedGame -> checkWin updatedGame p pos

                          

prompt:: Player -> String -> String
prompt p s = "Player " ++ show p ++ s

displayGame :: OuterGame -> IO ()
displayGame grid = putStrLn $ concat ((map (unlines)) (map (map (intercalate " || ")) ((interleave rowList) (map(map(map showPlayer)) (map transpose(map(map fst) grid))))))

displayInner :: InnerGame -> String
displayInner rs = unlines . interleave "-----------" $ map showPlayer rs

showPlayer :: [Player] -> [Char]
showPlayer ps = concat $ interleave "|" $ map (\x -> " " ++ show x ++ " ") ps

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

horizontal :: String
horizontal = concat $ replicate 40 "-"

rowList :: [[String]]
rowList = replicate 2 (replicate 1 horizontal)

getNum :: String -> IO Int
getNum s = do putStrLn s
              ss <- getLine
              if ss /= [] && all isDigit ss then return (read ss)
                                            else 
                                                do putStrLn "Error: invalid number"
                                                   getNum s


--for testing
correctRow = [ ([[X,X,X],[X,X,X],[X,X,X]], X),([[O,O,O],[O,O,O],[O,O,O]],O),([[X,X,X],[X,X,X],[X,X,X]],X) ]
correctRow1 = [ ([[X,X,X],[X,X,X],[X,X,X]], X),([[E,E,E],[E,E,E],[E,E,E]],E),([[E,E,E],[E,E,E],[E,E,E]],E) ]
emptyRow = [ ([[E,E,E],[E,E,E],[E,E,E]], E),([[E,E,E],[E,E,E],[E,E,E]],E),([[E,E,E],[E,E,E],[E,E,E]],E) ]
emptyGrid = replicate 3 emptyRow
drawGame = [ ([[E,E,E],[E,E,E],[E,E,E]], E),([[E,E,E],[E,E,E],[E,E,E]],E),([[E,E,E],[E,E,E],[E,E,E]],E) ]
drawExample = replicate 3 drawGame
correctGrid = replicate 3 correctRow
correctGrid1 = replicate 3 correctRow1
simpleRow = [[[X,X,X],[X,X,X],[X,X,X]],[[O,O,O],[O,O,O],[O,O,O]],[[E,E,E],[E,E,E],[E,E,E]]]
simpleGrid = replicate 3 simpleRow

