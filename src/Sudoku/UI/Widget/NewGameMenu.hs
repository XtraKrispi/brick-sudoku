module Sudoku.UI.Widget.NewGameMenu where

import Brick (Widget, attrName, str, vBox, withAttr)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center, hCenter)
import Sudoku.Types (Difficulty (..))
import Sudoku.UI.Types (Name)

toString :: Difficulty -> String
toString Easy = "Easy"
toString Medium = "Medium"
toString Hard = "Hard"
toString Evil = "Evil"

next :: Difficulty -> Difficulty
next Easy = Medium
next Medium = Hard
next Hard = Evil
next Evil = Easy

prev :: Difficulty -> Difficulty
prev Easy = Evil
prev Medium = Easy
prev Hard = Medium
prev Evil = Hard

menu :: Difficulty -> Widget Name
menu selected =
  let
    menuItem item =
      withAttr
        (if item == selected then attrName "selected-menu-item" else mempty)
        $ str
        $ toString item
    menuItems = menuItem <$> [Easy .. Evil]
   in
    center $
      vBox $
        [ hCenter $ str "New Game"
        , hCenter $
            border $
              vBox
                menuItems
        ]