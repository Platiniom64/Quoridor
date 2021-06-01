# quoridor
This was a coursework done for University.
I simulated the board game Quoridor, and implemented an AI to play against.

There are different version of the game, with differences each time:

In the file "haskell-quoridor-basic", the AI has a limited search space (of depth 1), and uses a utility function of "the higher the better for the current player". 

In the file "haskell-quoridor-minimax-AB-Reed", the AI uses the minimax algorithm to choose a move. The utility function is "the higher the better for the AI". It has a search space of depth 4. Also, the search can be improved by using alpha-beta-pruning. In that file I also implemented the Reed player, which is an AI which starts with the Reed opening.

In the file "haskell-quoridor-4", I changed the rules so that four players can play at the same time. The AI was adapted to work in that scenario too.

In the file "haskell-quoridor-moves", I improved the rules of the game so that blocking a player's way is not allowed anymore. The AI in that version uses the minimax algorithm (and hence AB pruning can be used).
