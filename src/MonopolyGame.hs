module MonopolyGame where

import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.Environment
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Bitmap
-- Debug only
import Debug.Trace

import Types
import Const

------------------------------
-- Helper funcitons
------------------------------

-- Modify element of the list by index
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt _ _ [] = []
modifyAt n f (x : xs)
   | n == 0 = (f x) : xs
   | otherwise = x : modifyAt (n - 1) f xs

-- Replace element of the list by index
replaceAt :: Int -> a -> [a] -> [a]
replaceAt n newVal = modifyAt n (\ x -> newVal)

-- Sum of two vectors
(|+|) :: Vec -> Vec -> Vec 
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Get the player position on screen from board position (field id)
plrPos2vec :: Int -> Vec
plrPos2vec pos = vec
  where 
    vec = 
      boardCenterShift |+|
      boardTLCShift |+| 
      (fieldCoords !! pos) |+|
      playerFieldShift

-- Return current player info
getCurrentPlayer :: AppState -> PlayerState
getCurrentPlayer appState = (players appState) !! (currentPlayer appState)

-- Evaluate new current player's position according to dices values
getNewCurrentPos :: AppState -> Int
getNewCurrentPos appState = ((position curPlayer) + diceSum) `mod` fieldNum 
  where
    diceSum   = (fst (dicesValue appState)) + (snd (dicesValue appState))
    curPlayer = getCurrentPlayer appState

-- Change the position of current player
moveCurrentPlayer :: AppState -> Int -> AppState
moveCurrentPlayer appState pos = appState { players = updatedPlayers }
  where
    updatedPlayers = 
      modifyAt (currentPlayer appState) (\ plr -> plr { position = pos }) (players appState)

------------------------------
-- Main logic
------------------------------

-- Process Player's turn
makeTurn :: AppState -> AppState
makeTurn appState = passNextTurn . processMove . throwDices $ appState

-- Generate new dices values
throwDices :: AppState -> AppState
throwDices appState 
  | val1 == val2  = trace ("Dices: " ++ show (val1, val2) ++ " Doubles in a row: " ++ show (doublesInRow appState + 1)) appState { dicesValue = (val1, val2), randomGens = (gen1, gen2), doublesInRow = doublesInRow appState + 1 }
  | otherwise     = trace ("Dices: " ++ show (val1, val2)) appState { dicesValue = (val1, val2), randomGens = (gen1, gen2), doublesInRow = 0 }
  where
    -- Get new random dices values and generators.
    (val1, gen1) = randomR diceNumRange (fst (randomGens appState))
    (val2, gen2) = randomR diceNumRange (snd (randomGens appState))

-- Process current move
processMove :: AppState -> AppState
processMove appState = trace ("Players: " ++ (show $ players updState) ++ "\n") updState
  where 
    updState = moveCurrentPlayer appState (getNewCurrentPos appState)

-- Pass the turn to the next player in game
passNextTurn :: AppState -> AppState
passNextTurn appState
  | (doublesInRow appState) > 0 = appState
  | not (isPlaying nextPlayer)  = passNextTurn updState
  | isPlaying nextPlayer        = updState
  where 
    nextPlayerId = ((currentPlayer appState) + 1) `mod` (playerNumber appState)
    nextPlayer   = (players appState) !! nextPlayerId
    updState = appState { currentPlayer = nextPlayerId }

------------------------------
-- Graphics and Events
------------------------------

-- Draw an app
drawApp :: AppState -> Picture
drawApp appState = Pictures [boardPic, playersPic, infoPic]
  where 
    boardPic = Translate 
      (fst boardCenterShift) 
      (snd boardCenterShift) 
      (boardPicture appState)
    playersPic = Pictures (drawPlayers (players appState))
    infoPic = drawInfo appState

-- Draw board extras
drawBoard :: BoardState -> Picture
drawBoard _ = Blank

-- Draw players on the board
drawPlayers :: [PlayerState] -> [Picture]
drawPlayers [] = []
drawPlayers (plr : plrs) = (drawPlayers plrs) ++ [drawPlayer plr]

drawPlayer :: PlayerState -> Picture
drawPlayer player 
  | isPlaying player  = Translate x y pic
  | otherwise         = Blank
  where 
      (x, y)  = plrPos2vec (position player)
      pic     = playerPicture player

-- Draw game info and statistics
drawInfo :: AppState -> Picture
drawInfo appState = Pictures [dicesPic]
  where
    dicesPic  = Translate x y (drawDices (v1, v2) dicesPics)
    dicesPics = dicesPictures appState 
    (x, y)    = boardCenterShift
    (v1, v2)  = dicesValue appState
    
-- Draw dices images
drawDices :: (Int, Int) -> [Picture]-> Picture
drawDices (0, 0)   pics = Blank
drawDices (v1, v2) pics = Pictures [pic1, pic2]
  where
    pic1 = Translate (-x) (-y) (pics !! (v1 - 1))
    pic2 = Translate x y (pics !! (v2 - 1))
    (x, y) = dicesShift

-- Handle events
handleEvent :: Event -> AppState -> AppState
-- Make turn when Space is pressed
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) appState =
  makeTurn appState
-- Handle LMB click
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) appState = 
  trace ("x: " ++ show x  ++ " y: " ++ show y) appState
  
-- Ignore all other events.
handleEvent _ appState = appState

-- Simulation step (updates nothing)
updateApp :: Float -> AppState -> AppState
updateApp _ appState = appState
   
------------------------------
-- Main function for this app
------------------------------

-- Run game.
run :: IO ()
run = do 
  boardPic <- loadBMP boardImagePath
  player0Pic <- loadBMP player0ImagePath
  player1Pic <- loadBMP player1ImagePath
  player2Pic <- loadBMP player2ImagePath
  player3Pic <- loadBMP player3ImagePath
  player4Pic <- loadBMP player4ImagePath
  dice1Pic <- loadBMP dice1ImagePath
  dice2Pic <- loadBMP dice2ImagePath
  dice3Pic <- loadBMP dice3ImagePath
  dice4Pic <- loadBMP dice4ImagePath
  dice5Pic <- loadBMP dice5ImagePath
  dice6Pic <- loadBMP dice6ImagePath

  rndGen1 <- newStdGen
  rndGen2 <- newStdGen

  let initiatedPlayer0 = initialPlayer0 { playerPicture = player0Pic }
  let initiatedPlayer1 = initialPlayer1 { playerPicture = player1Pic }
  let initiatedPlayer2 = initialPlayer2 { playerPicture = player2Pic }
  let initiatedPlayer3 = initialPlayer3 { playerPicture = player3Pic }
  let initiatedPlayer4 = initialPlayer4 { playerPicture = player4Pic }

  let initiatedPlayers = [initiatedPlayer0, initiatedPlayer1, initiatedPlayer2, initiatedPlayer3, initiatedPlayer4]

  let dicesPics = [dice1Pic, dice2Pic, dice3Pic, dice4Pic, dice5Pic, dice6Pic]
  let initiatedAppState = initialAppState { 
      playerNumber = 5
    , boardPicture = boardPic
    , randomGens = (rndGen1, rndGen2) 
    , players = initiatedPlayers
    , dicesPictures = dicesPics
    }

  play display bgColor fps initiatedAppState drawApp handleEvent updateApp
