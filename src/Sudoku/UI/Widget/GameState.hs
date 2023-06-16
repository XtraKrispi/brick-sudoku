module Sudoku.UI.Widget.GameState where

import Brick (Widget, str)
import Sudoku.UI.Types (GameState (..), Name, PlayingState (PlayingState))
import Sudoku.UI.Widget.Grid qualified as Grid
import Sudoku.UI.Widget.MainMenu qualified as MainMenu
import Sudoku.UI.Widget.NewGameMenu qualified as NewGameMenu

gameState :: GameState -> Widget Name
gameState (MainMenu menuSelection) = MainMenu.menu menuSelection
gameState (NewGameMenu menuSelection) = NewGameMenu.menu menuSelection
gameState (Playing (PlayingState diff grid selected)) = Grid.grid grid selected
gameState LoadingGameMenu = str "Hello"