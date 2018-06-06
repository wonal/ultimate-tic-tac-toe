module Tests where
import Test.HUnit
import Board

--runTestTT allTests
allTests = TestList[innerPositionValidationTests, innerWinTests, innerGameMoveTest, outerGameMoveTest]

rowTestX :: Test
rowTestX = TestCase (assertEqual "X wins row," (X) (rowWin xRowWin))

rowTestO :: Test
rowTestO = TestCase (assertEqual "O wins row," (O) (rowWin oRowWin))

colTestX :: Test
colTestX = TestCase (assertEqual "X wins col," (X) (columnWin xColWin))

colTestO :: Test
colTestO = TestCase (assertEqual "O wins col," (O) (columnWin oColWin))

diagonalTestX :: Test
diagonalTestX = TestCase (assertEqual "X wins diagonal," (X) (diagonalWin xDiagWin))

diagonalTestO :: Test
diagonalTestO = TestCase (assertEqual "O wins diagonal," (O) (diagonalWin oDiagWin))

winTest1 :: Test
winTest1 = TestCase (assertEqual "O wins," (O) (win fullWin))

winTest2 :: Test
winTest2 = TestCase (assertEqual "X wins," (X) (win fullWin1))

winTest3 :: Test
winTest3 = TestCase (assertEqual "O wins," (O) (win win1))

winTest4 :: Test
winTest4 = TestCase (assertEqual "X wins," (X) (win win2))

drawTest1 :: Test
drawTest1 = TestCase (assertEqual "it's a draw," (E) (win draw))

drawTest2 :: Test
drawTest2 = TestCase (assertEqual "it's a draw," (E) (win draw1))

innerWinTests = TestList[TestLabel "rowTestX" rowTestX, TestLabel "rowTestO" rowTestO,
                    TestLabel "colTestX" colTestX, TestLabel "colTestO" colTestO,
                    TestLabel "diagTestX" diagonalTestX, TestLabel "diagTestO" diagonalTestO,
                    TestLabel "winTest1" winTest1, TestLabel "winTest2" winTest2,
                    TestLabel "winTest1" winTest3, TestLabel "winTest2" winTest4,
                    TestLabel "drawTest1" drawTest1, TestLabel "drawTest2" drawTest2
                    ]

-------------------------------------------------------------------------------------------------

boundsTest1 :: Test
boundsTest1 = TestCase (assertEqual "invalid position," (BoundsError) (validPosition emptygame 9))

boundsTest2 :: Test
boundsTest2 = TestCase (assertEqual "invalid position," (BoundsError) (validPosition emptygame (-1)))

boundsTest3 :: Test
boundsTest3 = TestCase (assertEqual "valid position," (None) (validPosition emptygame (0)))

boundsTest4 :: Test
boundsTest4 = TestCase (assertEqual "valid position," (None) (validPosition emptygame (8)))

occupyTest1 :: Test
occupyTest1 = TestCase (assertEqual "X occupying," (OccupiedError) (validPosition testoccupy (0)))

occupyTest2 :: Test
occupyTest2 = TestCase (assertEqual "O occupying," (OccupiedError) (validPosition testoccupy (4)))

innerPositionValidationTests = TestList[TestLabel "boundsTest1" boundsTest1, TestLabel "boundsTest2" boundsTest2,
                                        TestLabel "boundsTest3" boundsTest3, TestLabel "boundsTest4" boundsTest4,
                                        TestLabel "occupyTest1" occupyTest1, TestLabel "occupyTest2" occupyTest2
                                       ]

-----------------------------------------------------------------------------------------------------

oobMoveTest1 :: Test
oobMoveTest1 = TestCase (assertEqual "invalid move - oob" (Error BoundsError) (move emptygame 9 X))

oobMoveTest2 :: Test
oobMoveTest2 = TestCase (assertEqual "invalid move - oob" (Error BoundsError) (move emptygame (-1) O))

moveTest1 :: Test
moveTest1 = TestCase (assertEqual "valid move" (InProgress validMoveX) (move emptygame 0 X))

moveTest2 :: Test
moveTest2 = TestCase (assertEqual "valid move" (InProgress validMoveO) (move emptygame 8 O))

occupyMoveTest1 :: Test
occupyMoveTest1 = TestCase (assertEqual "invalid move - occupied" (Error OccupiedError) (move testoccupy 7 X))

occupyMoveTest2 :: Test
occupyMoveTest2 = TestCase (assertEqual "invalid move - occupied" (Error OccupiedError) (move testoccupy 2 O))

drawMoveTest1 :: Test
drawMoveTest1 = TestCase (assertEqual "invalid move - decided(draw)" (Decided) (move draw 4 X))

drawMoveTest2 :: Test
drawMoveTest2 = TestCase (assertEqual "invalid move - decided(draw)" (Decided) (move draw 6 O))

xWinMoveTest1 :: Test
xWinMoveTest1 = TestCase (assertEqual "invalid move - decided(X win)" (Decided) (move win2 2 O))

xWinMoveTest2 :: Test
xWinMoveTest2 = TestCase (assertEqual "invalid move - decided(X win)" (Decided) (move win2 2 X))

oWinMoveTest1 :: Test
oWinMoveTest1 = TestCase (assertEqual "invalid move - decided(O win)" (Decided) (move win1 1 X))

oWinMoveTest2 :: Test
oWinMoveTest2 = TestCase (assertEqual "invalid move - decided(O win)" (Decided) (move win1 1 O))

innerGameMoveTest = TestList [TestLabel "oobMoveTest1" oobMoveTest1, TestLabel "oobMoveTest2" oobMoveTest2,
                             TestLabel "moveTest1" moveTest1, TestLabel "moveTest2" moveTest2,
                             TestLabel "occupyMoveTest1" occupyMoveTest1, TestLabel "occupyMoveTest2" occupyMoveTest2,
                             TestLabel "drawMoveTest1" drawMoveTest1, TestLabel "drawMoveTest2" drawMoveTest2,
                             TestLabel "xWinMoveTest1" xWinMoveTest1, TestLabel "oWinMoveTest1" oWinMoveTest1,
                             TestLabel "xWinMoveTest2" xWinMoveTest2, TestLabel "oWinMoveTest2" oWinMoveTest2]

--------------------------------------------------------------------------------------------------------------------------

selectGameTest1 :: Test
selectGameTest1 = TestCase (assertEqual "invalid choice -- oob" (Error BoundsError) (selectGame (game, 0) 9 0 X))

selectGameTest2 :: Test
selectGameTest2 = TestCase (assertEqual "invalid choice -- oob" (Error BoundsError) (selectGame (game, 0) (-1) 0 O))

selectGameTest3 :: Test
selectGameTest3 = TestCase (assertEqual "valid choice: decided game -> free play" (InProgress updatedGameX) (selectGame (game1, 0) (2) (4) X))

selectGameTest4 :: Test
selectGameTest4 = TestCase (assertEqual "valid choice: decided game -> free play" (InProgress updatedGameO) (selectGame (game1, 0) (1) (8) O))

selectGameTest5 :: Test
selectGameTest5 = TestCase (assertEqual "valid choice: decided game -> free play -> decided game" (Decided) (selectGame (game1, 0) (3) (0) X))

selectGameTest6 :: Test
selectGameTest6 = TestCase (assertEqual "valid choice: decided game -> free play -> decided game" (Decided) (selectGame (game1, 0) (8) (1) O))

selectGameTest7 :: Test
selectGameTest7 = TestCase (assertEqual "valid choice: decided game -> free play -> invalid position" (Error BoundsError) (selectGame (game1, 0) (2) (9) X))

selectGameTest8 :: Test
selectGameTest8 = TestCase (assertEqual "valid choice: decided game -> free play -> invalid position" (Error BoundsError) (selectGame (game1, 0) (1) (-1) O))

selectGameTest9 :: Test
selectGameTest9 = TestCase (assertEqual "valid choice: decided game -> free play -> occupied position" (Error OccupiedError) (selectGame (testOccupyGame, 0) (5) (4) X))

selectGameTest10 :: Test
selectGameTest10 = TestCase (assertEqual "valid choice: decided game -> free play -> occupied position" (Error OccupiedError) (selectGame (testOccupyGame, 0) (7) (8) O))

selectGameTest11 :: Test
selectGameTest11 = TestCase (assertEqual "invalid choice -- rule constraint" (Error RuleError) (selectGame (game, 0) (3) 0 O))

selectGameTest12 :: Test
selectGameTest12 = TestCase (assertEqual "invalid choice -- rule constraint" (Error RuleError) (selectGame (game, 0) (7) 4 X))

selectGameTest13 :: Test
selectGameTest13 = TestCase (assertEqual "invalid choice -- rule constraint even with invalid position" (Error RuleError) (selectGame (game, 0) (3) 9 O))

selectGameTest14 :: Test
selectGameTest14 = TestCase (assertEqual "invalid choice -- rule constraint even with invalid position" (Error RuleError) (selectGame (game, 0) (7) (-1) X))

selectGameTest15 :: Test
selectGameTest15 = TestCase (assertEqual "valid choice but invalid position" (Error BoundsError) (selectGame (game, 3) (3) (9) X))

selectGameTest16 :: Test
selectGameTest16 = TestCase (assertEqual "valid choice but invalid position" (Error BoundsError) (selectGame (game, 3) (3) (-1) O))

selectGameTest17 :: Test
selectGameTest17 = TestCase (assertEqual "valid choice but occupied position" (Error OccupiedError) (selectGame (testOccupyGame, 5) (5) (4) X))

selectGameTest18 :: Test
selectGameTest18 = TestCase (assertEqual "valid choice but occupied position" (Error OccupiedError) (selectGame (testOccupyGame, 5) (5) (4) O))

selectGameTest19 :: Test
selectGameTest19 = TestCase (assertEqual "valid choice but decided" (Decided) (selectGame (game1, 0) (0) (0) X))

selectGameTest20 :: Test
selectGameTest20 = TestCase (assertEqual "valid choice but decided" (Decided) (selectGame (game1, 0) (0) (4) O))

selectGameTest21 :: Test
selectGameTest21 = TestCase (assertEqual "valid choice" (InProgress updatedGameX) (selectGame (game1, 2) (2) (4) X))

selectGameTest22 :: Test
selectGameTest22 = TestCase (assertEqual "valid choice" (InProgress updatedGameO) (selectGame (game1, 1) (1) (8) O))

outerGameMoveTest = TestList [TestLabel "selectGameTest1" selectGameTest1, TestLabel "selectGameTest2" selectGameTest2,
                              TestLabel "selectGameTest3" selectGameTest3, TestLabel "selectGameTest4" selectGameTest4,
                              TestLabel "selectGameTest5" selectGameTest5, TestLabel "selectGameTest6" selectGameTest6,
                              TestLabel "selectGameTest7" selectGameTest7, TestLabel "selectGameTest8" selectGameTest8,
                              TestLabel "selectGameTest9" selectGameTest9, TestLabel "selectGameTest10" selectGameTest10,
                              TestLabel "selectGameTest11" selectGameTest11, TestLabel "selectGameTest12" selectGameTest12,
                              TestLabel "selectGameTest13" selectGameTest13, TestLabel "selectGameTest14" selectGameTest14,
                              TestLabel "selectGameTest15" selectGameTest15, TestLabel "selectGameTest16" selectGameTest16,
                              TestLabel "selectGameTest17" selectGameTest17, TestLabel "selectGameTest18" selectGameTest18,
                              TestLabel "selectGameTest19" selectGameTest19, TestLabel "selectGameTest20" selectGameTest20,
                              TestLabel "selectGameTest21" selectGameTest21, TestLabel "selectGameTest22" selectGameTest22]
                             

-----------------------------------------------------------------------------------------------------------------------                           
oRowWin = [[E,E,E],[E,E,E],[O,O,O]]
xRowWin = [[E,E,E],[X,X,X],[E,E,E]]
oColWin = [[E,O,E],[E,O,E],[E,O,E]]
xColWin = [[E,E,X],[E,E,X],[E,E,X]]
oDiagWin = [[O,E,E],[E,O,E],[E,E,O]]
xDiagWin = [[E,E,X],[E,X,E],[X,E,E]]
fullWin = [[X,X,O],[O,O,X],[O,X,X]]
fullWin1 = [[X,O,X],[O,X,O],[X,O,X]]
win1 = [[O,O,O],[E,X,X],[O,X,X]]
win2 = [[O,E,X],[O,X,X],[E,O,X]]
draw = [[X,O,X],[O,O,X],[X,X,O]]
draw1 = [[O,X,O],[X,O,O],[X,O,X]]
emptygame = [[E,E,E],[E,E,E],[E,E,E]]
validMoveX = [[X,E,E],[E,E,E],[E,E,E]]
validMoveO = [[E,E,E],[E,E,E],[E,E,O]]
testoccupy = [[X,O,X],[O,O,X],[X,X,E]]

game = emptyGame (emptyGame E, E)
example1 = [ ([[E,E,E],[X,X,X],[E,E,E]], X),([[E,E,E],[E,E,E],[E,E,E]],E),([[E,E,E],[E,E,E],[E,E,E]],E) ]
game1 = [example1, example1, reverse example1]

updatedExampleX = [ ([[E,E,E],[X,X,X],[E,E,E]], X),([[E,E,E],[E,E,E],[E,E,E]],E),([[E,E,E],[E,X,E],[E,E,E]],E) ]
updatedGameX = [updatedExampleX, example1, reverse example1]
updatedExampleO = [ ([[E,E,E],[X,X,X],[E,E,E]], X),([[E,E,E],[E,E,E],[E,E,O]],E),([[E,E,E],[E,E,E],[E,E,E]],E) ]
updatedGameO = [updatedExampleO, example1, reverse example1]
testOccupyGame = [example1, updatedExampleX, reverse updatedExampleO]
example2 = [ ([[X,O,E],[X,X,X],[E,O,O]], X),([[O,O,X],[O,X,X],[O,X,O]],E),([[O,O,O],[X,X,X],[O,O,O]],O) ]
game2 = [example2, example2, reverse example2]

blah = [ [([[E,E,E],[E,E,E],[E,E,E]], E),([[E,E,E],[O,O,E],[E,E,E]],E),([[E,E,E],[E,E,E],[E,E,E]],E)],
         [([[E,E,E],[E,E,E],[E,E,E]], E),([[E,E,E],[E,E,E],[E,E,O]],E),([[E,E,E],[E,E,E],[E,E,E]],E)],
         [([[E,E,E],[E,E,E],[E,E,E]], E),([[E,E,E],[E,E,E],[E,E,O]],E),([[E,E,E],[E,E,E],[E,E,E]],E)] ]