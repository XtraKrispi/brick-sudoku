module Sudoku.UI.Widget.Grid where

import Brick (Widget, attrName, hBox, str, vBox, withAttr)
import Brick.Widgets.Border (border)
import Data.List.Split (chunksOf)
import GHC.Arr (assocs)
import Sudoku.Types (Cell (..), Coord, UnfilledGrid, unGrid)
import Sudoku.UI.Types (Name)

toString :: Cell -> String
toString (Possible _) = " "
toString (Filled num) = show num
toString (Static num) = show num

row :: Coord -> [(Coord, Cell)] -> Widget Name
row selected cells = hBox $ (\(coord, cell) -> border . (withAttr (if selected == coord then attrName "selected" else mempty)) . str . toString $ cell) <$> cells

grid :: UnfilledGrid -> Coord -> Widget Name
grid g selected =
  let rows = chunksOf 9 $ assocs $ unGrid g
   in vBox (row selected <$> rows)