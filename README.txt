A Haskell implementation of Ultimate Tic-Tac-Toe

Build instructions:  
On Windows, run a "cabal update" command to ensure you have the Gloss library.
Create the executable using "ghc --make Main.hs". 
                     
How to use:  
There is only one version -- the GUI version, so launch and start playing!  
Player X goes first by default.  As far as terminology which might be 
useful for messages, I've deemed the sections of the larger outer tic-tac-toe
board a "cell" whereas an inner tic-tac-toe game is made up of "positions."  
Regardless of the outer or inner games, they are all referenced by numbers 1-9:
             1-2-3
             4-5-6
             7-8-9
 