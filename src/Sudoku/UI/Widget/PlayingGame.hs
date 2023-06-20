module Sudoku.UI.Widget.PlayingGame where

import Brick (Widget)
import Sudoku.UI.Types (Name, PlayingState (PlayingState))
import Sudoku.UI.Widget.Grid qualified as Grid

playingGame :: PlayingState -> Widget Name
playingGame (PlayingState _ grid selected) = Grid.grid grid selected