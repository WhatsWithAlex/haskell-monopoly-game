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
    movedState = case doublesInRow appState of
      3 -> jailCurPlayer appState
      _ -> moveCurrentPlayer appState (getNewCurrentPos appState)
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
        else if enoughCurProperty appState amount
        then
          appState
        else
          setCurPlayerStatus appState Bankrupt
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
  | status nextPlayer == Jailed   = updState
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
drawApp appState = Pictures [boardPic, fieldsPic, playersPic, infoPic]
  where 
    boardPic = Translate 
      (fst boardCenterShift) 
      (snd boardCenterShift) 
      (boardPicture appState)
    fieldsPic   = Pictures (map drawField (fields appState))
    playersPic  = Pictures (map drawPlayer (players appState))
    infoPic     = drawInfo appState

-- Draw information about field on board
drawField :: BoardField -> Picture
drawField field = case fieldType field of
  Property propertyField -> 
    if ownerId propertyField /= -1 
    then 
      Color clr (Translate (x + w / 2 + 1) (y + h / 2) (rectangleWire w (-h)))
    else
      Blank
    where
      clr = playersColors !! (ownerId propertyField)
      (x, y) = (fieldId2Vec (fieldId field))
      (w, h) = snd (fieldRects !! (fieldId field))
  _ -> Blank

-- Draw player figure on board
drawPlayer :: PlayerState -> Picture
drawPlayer player 
  | status player == Jailed    = Translate xJail yJail pic
  | status player /= Bankrupt  = Translate x y pic
  | otherwise                  = Blank
  where 
      (x, y)  = (fieldId2Vec (position player)) |+| offset
      (xJail, yJail) = (fieldId2Vec jailFieldId) |+| jailedShift |+| offset
      pic     = playerPicture player
      offset  = playersFieldShift !! (playerId player)

-- Draw game info and statistics
drawInfo :: AppState -> Picture
drawInfo appState = Pictures [dicesPic, playersStatsPic]
  where
    dicesPic  = Translate xDices yDices (drawDices (v1, v2) dicesPics)
    dicesPics = dicesPictures appState 
    (v1, v2)  = dicesValue appState
    playersStatsPic = 
      Translate xStats yStats
      (Pictures (map drawPlayerStats (players appState)))
    (xDices, yDices) = boardCenterShift
    (xStats, yStats) = statsShift
    
-- Draw dices images
drawDices :: (Int, Int) -> [Picture] -> Picture
drawDices (0, 0)   pics = Blank
drawDices (v1, v2) pics = Pictures [pic1, pic2]
  where
    pic1 = Translate (-x) (-y) (pics !! (v1 - 1))
    pic2 = Translate x y (pics !! (v2 - 1))
    (x, y) = dicesShift

-- Draw player balance and status info
drawPlayerStats :: PlayerState -> Picture
drawPlayerStats player = 
  Translate x y statText
  where
    (x, y)      = playersStatsShift !! id
    statText    = Color clr (Text statStr)
    statStr     = 
      "Player" ++ idStr ++ ": " ++ balanceStr ++ " Status: " ++ statusStr
    idStr       = show (id + 1)
    balanceStr  = show (balance player)
    statusStr   = show (status player)
    clr         = playersColors !! id
    id          = playerId player


-- Handle events
handleEvent :: Event -> AppState -> AppState
-- Make turn when Space is pressed
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) appState = 
  trace ("Space: " ++ (show $ players appState) ++ "\n") (startTurn appState)
-- Ready / Agree to pay
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) appState = 
  trace ("Enter: " ++ (show $ players appState) ++ "\n") (payMoney appState)
-- Agree to buy property by pressing 'y'
handleEvent (EventKey (Char 'y') Down _ _) appState =
  trace ("Y: " ++ (show $ players appState) ++ "\n") (buyProperty appState True)
-- Disagree to buy property by pressing 'n'
handleEvent (EventKey (Char 'n') Down _ _) appState =
  trace ("N: " ++ (show $ players appState) ++ "\n") (buyProperty appState False)
-- Upgrade property by clicking LMB on it
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) appState = 
  trace ("(x, y): " ++ show (x, y) ++ " fieldId: " ++ show (vec2FieldId (x, y))) appState
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
