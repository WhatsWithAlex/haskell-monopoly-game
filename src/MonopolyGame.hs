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
import Helpers

------------------------------
-- Main logic
------------------------------

-- Start player's turn
startTurn :: AppState -> AppState
startTurn appState
  | status curPlayer == Playing = passNextTurn . processMove . throwDices $ appState
  | otherwise                   = appState
  where 
    curPlayer = getCurrentPlayer appState

-- Generate new dices values
throwDices :: AppState -> AppState
throwDices appState 
  | val1 == val2 = 
    appState 
    { dicesValue = newDices
    , randomGens = newGens
    , doublesInRow = doublesInRow appState + 1 
    }
  | otherwise = 
    appState 
    { dicesValue = newDices
    , randomGens = newGens
    , doublesInRow = 0 
    }
  where
    -- Get new random dices values and generators.
    newDices      = (val1, val2)
    newGens       = (gen1, gen2)
    (val1, gen1)  = randomR diceNumRange (fst (randomGens appState))
    (val2, gen2)  = randomR diceNumRange (snd (randomGens appState))

-- Process current move
processMove :: AppState -> AppState
processMove appState = trace ("Players: " ++ (show $ players updState) ++ "\n") updState
  where       
    updState = case fieldType of 
      Property propertyField -> 
        if isMortgaged propertyField
        then 
          movedState
        else if ownerId propertyField == currentPlayerId appState
        then
          movedState
        else if hasOwner propertyField
        then
          setCurPlayerStatus movedState Paying
        else
          setCurPlayerStatus movedState Buying
      Policeman   -> jailCurPlayer movedState
      Tax amount  -> setCurPlayerStatus movedState Paying
      _           -> movedState
    movedState = moveCurrentPlayer appState (getNewCurrentPos appState)
    fieldType = getFieldType movedState (position curPlayer)
    curPlayer = getCurrentPlayer movedState

-- Process buying property
buyProperty :: AppState -> Bool -> AppState
buyProperty appState isBuy
  | status curPlayer /= Buying  = appState
  | isBuy                       = passNextTurn buyState
  | otherwise                   = passNextTurn aucState
  where   
    buyState  = case fieldType of
      Property propertyField -> 
        if enoughCurBalance appState (buyPrice propertyField)
        then 
          setCurPlayerStatus updState Playing
        else
          appState
        where 
          updState = 
            addCurPlayerProperty 
            (decreaseCurPlayerBalance appState (buyPrice propertyField)) 
            (position curPlayer)
      _ -> appState  
    fieldType = getFieldType appState (position curPlayer)
    curPlayer = getCurrentPlayer appState
    aucState  = setCurPlayerStatus appState Playing
  
-- Process paying rent / tax 
payMoney :: AppState -> AppState
payMoney appState
  | status curPlayer /= Paying  = appState
  | otherwise                   = passNextTurn payState
  where   
    payState  = case fieldType of
      Property propertyField -> 
        if enoughCurBalance appState rentAmount
        then 
          setCurPlayerStatus updState Playing
        else if enoughCurProperty appState rentAmount
        then
          appState
        else 
          setCurPlayerStatus appState Bankrupt
        where 
          updState   = payToPlayer appState (ownerId propertyField) rentAmount 
          rentAmount = getRentAmount appState propertyField
      Tax amount -> 
        if enoughCurBalance appState amount
        then 
          setCurPlayerStatus updState Playing
        else
          appState
        where 
          updState = decreaseCurPlayerBalance appState amount
    fieldType = getFieldType appState (position curPlayer)
    curPlayer = getCurrentPlayer appState
    aucState  = setCurPlayerStatus appState Playing

-- Pass the turn to the next player in game
passNextTurn :: AppState -> AppState
passNextTurn appState
  | doublesInRow appState > 0     = appState
  | status curPlayer  == Buying   = appState
  | status curPlayer  == Paying   = appState
  | status curPlayer  == Jailed   = passNextTurn updState
  | status nextPlayer == Bankrupt = passNextTurn updState
  | status nextPlayer == Playing  = updState
  | otherwise                     = passNextTurn updState
  where 
    updState      = appState { currentPlayerId = nextPlayerId }
    curPlayer     = getCurrentPlayer appState
    nextPlayer    = (players appState) !! nextPlayerId
    nextPlayerId  = 
      ((currentPlayerId appState) + 1) `mod` (playerNumber appState)

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
drawFields :: [BoardField] -> Picture
drawFields _ = Blank

-- Draw players on the board
drawPlayers :: [PlayerState] -> [Picture]
drawPlayers [] = []
drawPlayers (plr : plrs) = (drawPlayers plrs) ++ [drawPlayer plr]

drawPlayer :: PlayerState -> Picture
drawPlayer player 
  | status player == Jailed    = Translate xJail yJail pic
  | status player /= Bankrupt  = Translate x y pic
  | otherwise                  = Blank
  where 
      (x, y)  = fieldId2Vec (position player)
      pic     = playerPicture player
      (xJail, yJail) = (fieldId2Vec jailFieldId) |+| jailedShift

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
  startTurn appState
-- Ready / Agree to pay
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) appState = 
  payMoney appState
-- Agree to buy property
handleEvent (EventKey (Char 'y') Down _ _) appState =
  buyProperty appState True
-- Disagree to buy property
handleEvent (EventKey (Char 'n') Down _ _) appState =
  buyProperty appState False
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
