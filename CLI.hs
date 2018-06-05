--Old CLI code


ultimatetictactoe :: IO ()
ultimatetictactoe = takeTurn (emptyGame (emptyGame E, E)) O Nothing


checkWinCondition :: OuterGame -> Player -> Int -> IO ()
checkWinCondition g p c  | turnResult == X = putStrLn "Player X wins!\n"
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
                     case selectGame (g, c') cell pos p of 
                          Error BoundsError      -> do putStrLn ("Error: " ++ show pos ++ " is an invalid move as it must be within the board.")
                                                       takeTurn g p c
                          Error OccupiedError    -> do putStrLn ("Error: " ++ show pos ++ " is an invalid move.  That space is already occupied.")
                                                       takeTurn g p c
                          Error RuleError        -> do putStrLn ("Error: per the rules, " ++ show cell ++ " is an invalid game cell choice.  You must pick game cell " ++ show (maybe 0 id c) ++ ".")
                                                       takeTurn g p c 
                          Decided                -> do putStrLn ("That game cell has already been decided!  You can play in any free game now.")
                                                       takeTurn g p Nothing 
                          InProgress updatedGame -> checkWinCondition updatedGame p pos


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
