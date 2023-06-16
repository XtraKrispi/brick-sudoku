module Sudoku.Data where

import Sudoku.Types (UnfilledGrid)
import Sudoku.Utils (readGrid)

easy1 :: UnfilledGrid
easy1 = processPuzzle "91...8.2..8734.9.525..97....7...5.3.1.2.34..8..82.97.4....5.3.......32.634..268.1"

medium1 :: UnfilledGrid
medium1 = processPuzzle ".6192....27....6......162.3..7....85..5......136857.......9.5..6.25.48...5.36...7"

evil1 :: UnfilledGrid
evil1 = processPuzzle ".6..1...55.8.3..4..2...8...6...........1...2.3.5..9..11.9..3..2.8...........4.7.."

processPuzzle :: String -> UnfilledGrid
processPuzzle s =
  case readGrid s of
    Just g -> g
    Nothing -> error "I've done something wrong here"
