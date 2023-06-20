module Sudoku.UI.Widget.GameState where

import Brick (Widget, str)
import Sudoku.UI.Types (GameState (..), Name)
import Sudoku.UI.Widget.MainMenu qualified as MainMenu
import Sudoku.UI.Widget.NewGameMenu qualified as NewGameMenu
import Sudoku.UI.Widget.PlayingGame qualified as PlayingGame

gameState :: GameState -> Widget Name
gameState (MainMenu menuSelection) = MainMenu.menu menuSelection
gameState (NewGameMenu menuSelection) = NewGameMenu.menu menuSelection
gameState (Playing st) = PlayingGame.playingGame st
gameState LoadingGameMenu = str "Hello"