module Sudoku.Solver where

import Control.Applicative ((<|>))
import Data.List ((\\), minimumBy)
import Data.Maybe (catMaybes)
import Sudoku.Domain (getNeighbors, isGridFilled, isInvalidGrid, updateGrid, getRow, getColumn, getBox, numberOfPossibilities, isPossible)
import GHC.Arr ((!), (//), assocs)
import Sudoku.Types (Cell (..), Coord, FilledGrid, Grid (..), Number (..), RowColBox (..), UnfilledGrid)

solve :: UnfilledGrid -> Maybe FilledGrid
solve grid = go grid >>= updateGrid
 where
  go g =
    pruneGrid g >>= solve'
  solve' g
    | isInvalidGrid g = Nothing
    | isGridFilled g = Just g
    | otherwise =
        let (grid1, grid2) = nextGrids g
         in go grid1 <|> go grid2

solveMultiple :: UnfilledGrid -> [FilledGrid]
solveMultiple grid = catMaybes $ updateGrid <$> go grid
 where
  go g = case pruneGrid g of
    Just g' -> solve' g'
    Nothing -> []
  solve' g
    | isInvalidGrid g = []
    | isGridFilled g = [g]
    | otherwise =
        let (grid1, grid2) = nextGrids g
         in go grid1 ++ go grid2

getAvailableNumbers :: UnfilledGrid -> Coord -> [Number]
getAvailableNumbers (Grid grid) coord =
  let RowColBox row col box = (grid !) <$> getNeighbors coord
      allValues = [n| Filled n <- row ++ col ++ box]
   in [One .. Nine] \\ allValues

hasMultipleSolutions :: UnfilledGrid -> Bool
hasMultipleSolutions = (> 1) . length . take 2 . solveMultiple

hasOneSolution :: UnfilledGrid -> Bool
hasOneSolution = (== 1) . length . take 2 . solveMultiple

pruneCells :: [(Coord, Cell)] -> Maybe [(Coord, Cell)]
pruneCells cells = traverse pruneCell cells
 where
  filled = [x | Filled x <- (snd <$> cells)]

  pruneCell (coord, (Possible xs)) =
    case xs \\ filled of
      [] -> Nothing
      [y] -> Just $ (coord, Filled y)
      ys -> Just $ (coord, Possible ys)
  pruneCell x = Just x

pruneRows :: UnfilledGrid -> Maybe UnfilledGrid
pruneRows grid@(Grid g) =
  Grid . (g //) . concat <$> (traverse pruneCells $ flip getRow grid <$> [0 .. 8])

pruneCols :: UnfilledGrid -> Maybe UnfilledGrid
pruneCols grid@(Grid g) =
  Grid . (g //) . concat <$> (traverse pruneCells $ flip getColumn grid <$> [0 .. 8])

pruneBoxes :: UnfilledGrid -> Maybe UnfilledGrid
pruneBoxes grid@(Grid g) =
  Grid . (g //) . concat <$> (traverse pruneCells $ flip getBox grid <$> [0 .. 8])

pruneGrid' :: UnfilledGrid -> Maybe UnfilledGrid
pruneGrid' g =
  pruneRows g >>= pruneCols >>= pruneBoxes

pruneGrid :: UnfilledGrid -> Maybe UnfilledGrid
pruneGrid =
  fixM pruneGrid'
 where
  fixM f x = f x >>= \x' -> if x' == x then return x else fixM f x'

nextGrids :: UnfilledGrid -> (UnfilledGrid, UnfilledGrid)
nextGrids (Grid g) =
  let smallestPossibilities =
        minimumBy
          ( \(_, c1) (_, c2) ->
              compare
                (numberOfPossibilities c1)
                (numberOfPossibilities c2)
          )
          $ filter (isPossible . snd)
          $ assocs g
      fixCell :: (Coord, Cell) -> (Coord, Cell, Cell)
      fixCell (c, (Possible [x, y])) = (c, Filled x, Filled y)
      fixCell (c, (Possible (x : xs))) = (c, Filled x, Possible xs)
      fixCell _ = error "Ruh-roh, this should never happen"

      (coord, left, right) = fixCell smallestPossibilities
   in (Grid $ g // [(coord, left)], Grid $ g // [(coord, right)])  