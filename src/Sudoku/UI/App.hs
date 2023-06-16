{-# LANGUAGE OverloadedRecordDot #-}

module Sudoku.UI.App where

import Brick (App (App, appChooseCursor, appHandleEvent, appStartEvent), AttrMap, BrickEvent (..), EventM, appAttrMap, appDraw, attrMap, attrName, defaultMain, get, halt, on, put)
import Control.Monad.IO.Class (liftIO)
import Graphics.Vty (Attr, Event (..), Key (..), black, white)
import Sudoku.Domain (nextCol, nextRow, prevCol, prevRow)
import Sudoku.Loader qualified as Loader
import Sudoku.Types (Difficulty (Easy))
import Sudoku.UI.Types (GameState (MainMenu, NewGameMenu, Playing), Handle (..), Menu (..), Name, PlayingState (..))
import Sudoku.UI.Widget.GameState qualified as Widget.GameState
import Sudoku.UI.Widget.MainMenu qualified as MainMenu
import Sudoku.UI.Widget.NewGameMenu qualified as NewGameMenu

handleEvent :: Handle -> BrickEvent Name e -> EventM Name GameState ()
handleEvent _ (VtyEvent (EvKey KEsc [])) =
  do
    gameState <- get
    case gameState of
      MainMenu _ -> halt
      _ -> put $ MainMenu NewGame
handleEvent _ (VtyEvent (EvKey KDown [])) = do
  gameState <- get
  case gameState of
    MainMenu menu ->
      put (MainMenu (MainMenu.next menu))
    NewGameMenu menu ->
      put (NewGameMenu (NewGameMenu.next menu))
    Playing playingState ->
      put ((Playing playingState{selectedCell = nextRow playingState.selectedCell}))
    _ -> pure ()
handleEvent _ (VtyEvent (EvKey KUp [])) = do
  gameState <- get
  case gameState of
    MainMenu menu ->
      put (MainMenu (MainMenu.prev menu))
    NewGameMenu menu ->
      put (NewGameMenu (NewGameMenu.prev menu))
    Playing playingState ->
      put ((Playing playingState{selectedCell = prevRow playingState.selectedCell}))
    _ -> pure ()
handleEvent _ (VtyEvent (EvKey KLeft [])) = do
  gameState <- get
  case gameState of
    Playing playingState ->
      put ((Playing playingState{selectedCell = prevCol playingState.selectedCell}))
    _ -> pure ()
handleEvent _ (VtyEvent (EvKey KRight [])) = do
  gameState <- get
  case gameState of
    Playing playingState ->
      put ((Playing playingState{selectedCell = nextCol playingState.selectedCell}))
    _ -> pure ()
handleEvent handle (VtyEvent (EvKey KEnter [])) = do
  gameState <- get
  case gameState of
    MainMenu menu ->
      case menu of
        NewGame -> put $ NewGameMenu Easy
        ExitGame -> halt
        _ -> pure ()
    NewGameMenu difficulty -> do
      mSudoku <- liftIO $ handle.getNewSudoku difficulty
      case mSudoku of
        Just grid -> put $ Playing (PlayingState difficulty grid (0, 0))
        Nothing -> pure () -- What to do here?
    _ -> pure ()
handleEvent _ _ = pure ()

defaultAttribute :: Attr
defaultAttribute = white `on` black

getAttrMap :: GameState -> AttrMap
getAttrMap _ =
  attrMap
    defaultAttribute
    [ (attrName "selected-menu-item", black `on` white)
    , (attrName "selected", black `on` white)
    ]

app :: Handle -> App GameState e Name
app handle =
  App
    { appDraw = pure . Widget.GameState.gameState
    , appChooseCursor = \_ _ -> Nothing
    , appHandleEvent = handleEvent handle
    , appStartEvent = pure ()
    , appAttrMap = getAttrMap
    }

run :: IO ()
run = do
  let handle =
        Handle
          { getNewSudoku = Loader.loadSudoku
          }
  _ <- defaultMain (app handle) ((MainMenu NewGame))
  pure ()