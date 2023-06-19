module Sudoku.UI.Widget.MainMenu where

import Brick (Widget, attrName, str, vBox, withAttr)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center, hCenter)
import Sudoku.UI.Types (Menu (..), Name)

toString :: Menu -> String
toString NewGame = "New Game"
toString LoadGame = "Load Game"
toString ExitGame = "Exit"

next :: Menu -> Menu
next NewGame = LoadGame
next LoadGame = ExitGame
next ExitGame = NewGame

prev :: Menu -> Menu
prev NewGame = ExitGame
prev LoadGame = NewGame
prev ExitGame = LoadGame

menu :: Menu -> Widget Name
menu selected =
  let
    menuItem item =
      withAttr
        (if item == selected then attrName "selected-menu-item" else mempty)
        $ str
        $ toString item
    menuItems = menuItem <$> [NewGame .. ExitGame]
   in
    center $
      vBox $
        [ hCenter $ str "Sudoku"
        , hCenter $
            border $
              vBox
                menuItems
        ]