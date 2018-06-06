Build instructions:  I have been working on Windows and have just been using "ghc --make Main.hs" to build my project.  
                     I used Gloss for my GUI, so "cabal update" should be sufficient, but I'm assuming you're already updated.


How to use:  There is only one version -- the GUI version, so launch and start playing!  Player X goes first by default.  As 
             far as terminology which might be useful for my messages, I've deemed the sections of the larger outer tic-tac-toe
             board a "cell" whereas an inner tic-tac-toe game is made up of "positions."  Regardless of the outer or inner games, they 
             are all referenced by numbers 1-9:
             1-2-3
             4-5-6
             7-8-9
             Nothing fancy with features :P  I've added a visual cue to help show in which cell a player is supposed to play in next per
             the rules, and of course they have free pick if the game they're directed to is decided already.  