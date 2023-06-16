{-# LANGUAGE TemplateHaskell #-}

module Sudoku.UI.Types where

import Sudoku.Types (Coord, Difficulty, UnfilledGrid)

data Name = Name
  deriving (Eq, Ord)

data Menu = NewGame | LoadGame | ExitGame
  deriving (Show, Eq, Enum, Bounded)

data PlayingState = PlayingState
  { difficulty :: Difficulty
  , grid :: UnfilledGrid
  , selectedCell :: Coord
  }
  deriving (Show, Eq)

data GameState
  = MainMenu Menu
  | LoadingGameMenu
  | NewGameMenu Difficulty
  | Playing PlayingState
  deriving (Show, Eq)

data Handle = Handle
  { getNewSudoku :: Difficulty -> IO (Maybe UnfilledGrid)
  }