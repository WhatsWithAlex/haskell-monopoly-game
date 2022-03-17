module MonopolyGame where

import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.Environment
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Bitmap

import Types
import Const


-- Draw an app
drawApp :: AppState -> Picture
drawApp appState = Translate (-1000) 0 (Scale 0.45 0.45 (boardPicture appState))

-- Draw board extras
drawBoard :: BoardState -> Picture
drawBoard _ = Blank

-- Draw players on the board
drawPlayers :: [PlayerState] -> Picture
drawPlayers _ = Blank

-- Draw players info for app
drawPlayersInfo :: [PlayerState] -> Picture
drawPlayersInfo _ = Blank

-- Handle events
handleEvent :: Event -> AppState -> AppState
handleEvent _ appState = appState

-- Simulation step (updates nothing)
updateApp :: Float -> AppState -> AppState
updateApp _ appState = appState

-- Process Player's turn
turn :: AppState -> PlayerState -> AppState
turn appState _ = appState

------------------------------
-- Main function for this app
------------------------------

-- Run game. This is the ONLY unpure function
run :: IO ()
run = do 
  boardPic <- loadBMP boardImagePath
  let initiatedAppState = initialAppState { boardPicture = boardPic }
  play display bgColor fps initiatedAppState drawApp handleEvent updateApp
