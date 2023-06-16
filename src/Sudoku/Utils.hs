module Sudoku.Utils where

import Data.List (intercalate, (\\))
import Data.List qualified
import Data.List.Split (chunksOf)
import Data.Map (Map, elems, insert, singleton, (!))
import GHC.Arr (listArray, (//))
import GHC.Arr qualified as A
import Sudoku.Types (Cell (..), Coord, FilledGrid, Grid (..), Number (..), UnfilledGrid)
import System.Random (Random (randomR), RandomGen)

fisherYatesStep :: (RandomGen g) => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
 where
  (j, gen') = randomR (0, i) gen

shuffle :: (RandomGen g) => g -> [a] -> ([a], g)
shuffle gen [] = ([], gen)
shuffle gen l =
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
 where
  toElems (x, y) = (elems x, y)
  numerate = zip [1 ..]
  initial x gen' = (singleton 0 x, gen')

allPossible :: Cell
allPossible = Possible [One .. Nine]

mkEmptyGrid :: UnfilledGrid
mkEmptyGrid =
  Grid $ listArray ((0, 0), (8, 8)) $ take 81 (repeat allPossible)

readGrid :: String -> Maybe UnfilledGrid
readGrid xs =
  let Grid g = mkEmptyGrid
      convert '1' = Just One
      convert '2' = Just Two
      convert '3' = Just Three
      convert '4' = Just Four
      convert '5' = Just Five
      convert '6' = Just Six
      convert '7' = Just Seven
      convert '8' = Just Eight
      convert '9' = Just Nine
      convert _ = Nothing
   in fmap (Grid . (g //))
        $ sequence
        $ fmap
          ( \(idx, c) ->
              if idx < 81
                then case convert c of
                  Just n -> Just (toCoord idx, Filled n)
                  Nothing -> Nothing
                else Nothing
          )
        $ filter (\(_, c) -> c /= '.')
        $ zip [0 ..] xs

showGrid :: UnfilledGrid -> String
showGrid =
  unlines
    . map (unwords . map showCell)
    . chunksOf 9
    . A.elems
    . unGrid
 where
  showCell (Filled x) = show x
  showCell (Possible _) = "."

showGridString :: UnfilledGrid -> String
showGridString =
  concat
    . (map showCell)
    . A.elems
    . unGrid
 where
  showCell (Filled x) = show x
  showCell (Possible _) = "."

showFilledGrid :: FilledGrid -> String
showFilledGrid =
  unlines
    . map (unwords . map showCell)
    . chunksOf 9
    . A.elems
    . unGrid
 where
  showCell x = show x

showGridWithPossibilities :: UnfilledGrid -> String
showGridWithPossibilities =
  unlines
    . map (unwords . map showCell)
    . chunksOf 9
    . A.elems
    . unGrid
 where
  showCell (Filled x) = show x ++ "          "
  showCell (Possible xs) =
    (++ "]")
      . Data.List.foldl' (\acc x -> acc ++ if x `elem` xs then show x else " ") "["
      $ [One .. Nine]

toCoord :: Int -> Coord
toCoord idx = (idx `div` 9, idx `mod` 9)

-- | Is true iff a list has no duplicate elements.
nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x : xs) = x `notElem` xs && nodups xs

-- | Splits a list into multiple lists of a given length.
groupBy :: Int -> [a] -> [[a]]
groupBy n [] = []
groupBy n xs = (take n xs) : groupBy n (drop n xs)

-- | The reverse operation of groupBy.
ungroup :: [[a]] -> [a]
ungroup = concat

-- | Is true iff a given list contains exactly one element.
single :: [a] -> Bool
single [a] = True
single _ = False

-- | Removes the elements of the first list from the second list.
delete :: (Eq a) => [a] -> [a] -> [a]
delete = flip (\\)