{-# LANGUAGE DeriveAnyClass #-}

module Sudoku.Types where

import GHC.Arr (Array)
import System.Random (Random (random, randomR))

data Number
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Enum, Bounded, Ord, Eq)

instance Show Number where
  show One = "1"
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"

instance Random Number where
  randomR = enumRandomR
   where
    enumRandomR (a, b) g =
      case randomR (fromEnum a, fromEnum b) g of
        (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

data Cell = Possible [Number] | Filled Number
  deriving (Show, Eq)

type Coord = (Int, Int)

newtype Grid cell = Grid {unGrid :: Array Coord cell}
  deriving (Show, Eq)

type FilledGrid = Grid Number

type UnfilledGrid = Grid Cell

data Solution = OneSolution FilledGrid | MultipleSolutions | NoSolution
  deriving (Show)

data RowColBox coord = RowColBox
  { row :: [coord]
  , col :: [coord]
  , box :: [coord]
  }
  deriving (Show, Eq)

instance Functor RowColBox where
  fmap :: (a -> b) -> RowColBox a -> RowColBox b
  fn `fmap` (RowColBox row col box) =
    RowColBox (fn <$> row) (fn <$> col) (fn <$> box)

type Neighbors = RowColBox Coord

data Difficulty
  = Easy
  | Medium
  | Hard
  | Evil
  deriving (Show, Eq, Enum, Bounded)