{-# LANGUAGE OverloadedRecordDot #-}

module Sudoku.Domain where

import Data.List (nubBy)
import Data.Maybe (catMaybes)
import GHC.Arr (array, assocs, elems, indices, (!))
import Sudoku.Types (
  Cell (..),
  Coord,
  FilledGrid,
  Grid (Grid, unGrid),
  Neighbors,
  RowColBox (RowColBox),
  UnfilledGrid,
  box,
  col,
  row,
 )

getRow :: Int -> Grid cell -> [(Coord, cell)]
getRow row (Grid g) =
  filter (\((r, _), _) -> r == row) $ assocs g

getColumn :: Int -> Grid cell -> [(Coord, cell)]
getColumn col (Grid g) =
  filter (\((_, c), _) -> c == col) $ assocs g

getBox :: Int -> Grid cell -> [(Coord, cell)]
getBox boxNum (Grid g) =
  let tlRow = boxNum `div` 3 * 3
      tlCol = boxNum `mod` 3 * 3
      coords =
        [ (tlRow, tlCol)
        , (tlRow, tlCol + 1)
        , (tlRow, tlCol + 2)
        , (tlRow + 1, tlCol)
        , (tlRow + 1, tlCol + 1)
        , (tlRow + 1, tlCol + 2)
        , (tlRow + 2, tlCol)
        , (tlRow + 2, tlCol + 1)
        , (tlRow + 2, tlCol + 2)
        ]
   in filter (\(c, _) -> any (c ==) coords) $ assocs g

isPossible :: Cell -> Bool
isPossible (Possible _) = True
isPossible _ = False

numberOfPossibilities :: Cell -> Int
numberOfPossibilities (Possible xs) = length xs
numberOfPossibilities _ = 1

isGridFilled :: UnfilledGrid -> Bool
isGridFilled (Grid grid) = null [() | Possible _ <- elems grid]

isInvalidGrid :: UnfilledGrid -> Bool
isInvalidGrid g =
  any isInvalid (flip getRow g <$> [0 .. 8])
    || any isInvalid (flip getColumn g <$> [0 .. 8])
    || any isInvalid (flip getBox g <$> [0 .. 8])
 where
  isInvalid cells =
    let values = snd <$> cells
        filledNumbers = [x | Filled x <- values]
        emptyPossibles = [x | Possible x <- values, null x]
     in hasDups filledNumbers || not (null emptyPossibles)

  hasDups l = hasDups' l []

  hasDups' [] _ = False
  hasDups' (y : ys) xs
    | y `elem` xs = True
    | otherwise = hasDups' ys (y : xs)

updateGrid :: UnfilledGrid -> Maybe FilledGrid
updateGrid (Grid g)
  | all
      ( \c -> case c of
          Possible _ -> False
          _ -> True
      )
      (elems g) =
      Just $
        Grid $
          array ((0, 0), (8, 8)) $
            catMaybes $
              ( \(i, e) -> case e of
                  Possible _ -> Nothing
                  Filled n -> Just (i, n)
                  Static n -> Just (i, n)
              )
                <$> assocs g
  | otherwise = Nothing

getEmptyCells :: UnfilledGrid -> [Coord]
getEmptyCells =
  fmap fst
    . filter
      ( \(_, c) ->
          case c of
            Possible _ -> True
            _ -> False
      )
    . assocs
    . unGrid

getNeighbors :: Coord -> Neighbors
getNeighbors (row, col) =
  let (tlRow, tlCol) = (((row `div` 3) * 3), ((col `div` 3) * 3))
   in RowColBox
        { row = (row,) <$> [0 .. 8]
        , col = (,col) <$> [0 .. 8]
        , box =
            [ (tlRow, tlCol)
            , (tlRow, tlCol + 1)
            , (tlRow, tlCol + 2)
            , (tlRow + 1, tlCol)
            , (tlRow + 2, tlCol)
            , (tlRow + 1, tlCol + 1)
            , (tlRow + 2, tlCol + 2)
            ]
        }

-- Grid validation
isValidSolution :: FilledGrid -> Bool
isValidSolution = isValid (==)

isValidUnfilled :: UnfilledGrid -> Bool
isValidUnfilled =
  isValid
    ( \c1 c2 -> case (c1, c2) of
        (Possible _, Possible _) -> False
        _ -> c1 == c2
    )

isValid :: (Eq cell) => (cell -> cell -> Bool) -> Grid cell -> Bool
isValid fn grid =
  all (isValidNeighborhood fn grid) $ indices $ unGrid grid

isValidNeighborhood :: (Eq cell) => (cell -> cell -> Bool) -> Grid cell -> Coord -> Bool
isValidNeighborhood fn (Grid grid) coord =
  let neighbors = (grid !) <$> getNeighbors coord
      isUnique coords = coords == nubBy fn coords
   in isUnique neighbors.box
        && isUnique neighbors.row
        && isUnique neighbors.col

nextRow :: Coord -> Coord
nextRow (row, col)
  | row == 8 = (row, col)
  | otherwise = (row + 1, col)

prevRow :: Coord -> Coord
prevRow (row, col)
  | row == 0 = (row, col)
  | otherwise = (row - 1, col)

nextCol :: Coord -> Coord
nextCol (row, col)
  | col == 8 = (row, col)
  | otherwise = (row, col + 1)

prevCol :: Coord -> Coord
prevCol (row, col)
  | col == 0 = (row, col)
  | otherwise = (row, col - 1)