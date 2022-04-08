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
  | status curPlayer == Playing = 
    passNextTurn . processMove . throwDices $ appState
  | otherwise = 
    appState
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
processMove appState = updState
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
      _ -> 
        if newCurPos < (position curPlayer) 
        then 
          increaseCurPlayerBalance newState roundSalary
        else 
          newState
        where
          newState  = moveCurrentPlayer appState newCurPos
          newCurPos = getNewCurrentPos appState
          curPlayer = getCurrentPlayer appState
    fieldType = getFieldType movedState (position movedPlayer)
    movedPlayer = getCurrentPlayer movedState

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
  
-- Process paying rent / tax / releasing from jail
payMoney :: AppState -> AppState
payMoney appState
  | status curPlayer == Paying  = passNextTurn payState
  | status curPlayer == Jailed  = payState
  | otherwise                   = appState
  where   
    payState  = case fieldType of
      Property propertyField -> 
        checkAndPay appState (ownerId propertyField) rentAmount
        where
          rentAmount = getRentAmount appState propertyField
      Tax amount  -> checkAndPay appState (-1) amount
      Jail        -> checkAndPay appState (-1) releaseTax
    fieldType = getFieldType appState (position curPlayer)
    curPlayer = getCurrentPlayer appState
    aucState  = setCurPlayerStatus appState Playing

-- Process upgrading / lifting mortgaged property
makeUpgrade :: AppState -> Int -> AppState
makeUpgrade appState fieldId = case getFieldType appState fieldId of 
  Property propertyField -> case propertyType propertyField of 
    Street streetField -> 
      if 
        enoughCurBalance appState upgradePrice &&
        hasMonopoly appState (streetColor streetField) &&
        upgrades streetField == curMinUpgrade &&
        upgrades streetField < maxUpgrades
      then 
        upgradeStreet 
        (decreaseCurPlayerBalance appState upgradePrice)
        fieldId
      else if 
        enoughCurBalance appState liftPrice &&
        ownrId == currentPlayerId appState &&
        isMortgaged propertyField 
      then
        liftProperty
        (decreaseCurPlayerBalance appState liftPrice)
        fieldId
      else
        appState
      where 
        upgradePrice  = getUpgradePrice appState fieldId
        liftPrice     = price `div` 2 + price `div` 10
        price      = buyPrice propertyField
        curMinUpgrade = getMinUpgrade appState (streetColor streetField)
        ownrId        = ownerId (getProperty appState fieldId)
    _ ->
      if 
        enoughCurBalance appState liftPrice &&
        ownrId == currentPlayerId appState &&
        isMortgaged propertyField 
      then
        liftProperty
        (decreaseCurPlayerBalance appState liftPrice)
        fieldId
      else
        appState
      where 
        liftPrice     = price `div` 2 + price `div` 10
        price         = buyPrice propertyField
        ownrId        = ownerId (getProperty appState fieldId)
  _ -> appState

-- Process downgrading / mortgaging property
makeDowngrade :: AppState -> Int -> AppState
makeDowngrade appState fieldId = case getFieldType appState fieldId of 
  Property propertyField -> case propertyType propertyField of 
    Street streetField -> 
      if 
        ownrId == currentPlayerId appState &&
        upgrades streetField == maxUpgrade &&
        upgrades streetField > 0
      then 
        downgradeStreet 
        (increaseCurPlayerBalance appState downgradePayment)
        fieldId
      else if 
        ownrId == currentPlayerId appState &&
        not (isMortgaged propertyField)
      then
        mortgageProperty
        (increaseCurPlayerBalance appState mortgagePayment)
        fieldId
      else 
        appState
      where
        maxUpgrade        = getMaxUpgrade appState (streetColor streetField)
        downgradePayment  = (getUpgradePrice appState fieldId) `div` 2
        mortgagePayment   = (buyPrice propertyField) `div` 2
        ownrId            = ownerId (getProperty appState fieldId)
    _ ->
      if 
        ownrId == currentPlayerId appState &&
        not (isMortgaged propertyField)
      then
        mortgageProperty
        (increaseCurPlayerBalance appState mortgagePayment)
        fieldId
      else 
        appState
      where
        mortgagePayment   = (buyPrice propertyField) `div` 2
        ownrId            = ownerId (getProperty appState fieldId)
  _ -> appState

-- Pass the turn to the next player in game
passNextTurn :: AppState -> AppState
passNextTurn appState
  | status curPlayer  == Jailed   = passNextTurn updState
  | doublesInRow appState > 0     = appState
  | status curPlayer  == Buying   = appState
  | status curPlayer  == Paying   = appState
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
drawApp appState = 
  Pictures [boardPic, fieldsPic, playersPic, infoPic, upgrades]
  where 
    boardPic = Translate 
      (fst boardCenterShift) 
      (snd boardCenterShift) 
      (boardPicture appState)
    fieldsPic   = Pictures (map drawField (fields appState))
    playersPic  = Pictures (map drawPlayer (players appState))
    infoPic     = drawInfo appState
    upgrades    = 
      Pictures
      (map (\ field -> drawFieldUpgrades appState field) (fields appState))

-- Draw information about field on board
drawField :: BoardField -> Picture
drawField field = case fieldType field of
  Property propertyField -> 
    if ownerId propertyField /= -1 
    then 
      if isMortgaged propertyField 
      then 
        Color clr (Pictures [fieldPic, mortgagedPic])
      else 
        Color clr fieldPic
    else
      Blank
    where
      fieldPic = (Translate (x + w / 2 + 1) (y + h / 2) (rectangleWire w (-h)))
      mortgagedPic = Line [(x, y), (x + w, y + h)]
      clr = playersColors !! (ownerId propertyField)
      (x, y) = (fieldId2Vec (fieldId field))
      (w, h) = snd (fieldRects !! (fieldId field))
  _ -> Blank

-- Draw street upgrades
drawFieldUpgrades :: AppState -> BoardField -> Picture
drawFieldUpgrades appState field = case fieldType field of
  Property propertyField -> case propertyType propertyField of
    Street streetField -> case sideNum of
      0 -> 
        Translate x y
        (drawUpgrades (housePic, hotelPic) (upgrades streetField))
      1 ->
        Translate x y
        (Rotate 90.0
        (drawUpgrades (housePic, hotelPic) (upgrades streetField)))
      2 ->
        Translate x y
        (drawUpgrades (housePic, hotelPic) (upgrades streetField))
      3 ->
        Translate x y
        (Rotate 270.0
        (drawUpgrades (housePic, hotelPic) (upgrades streetField)))
    _ -> Blank
  _ -> Blank
  where 
    (x, y) = fieldId2Vec (fieldId field) |+| (houseUpgradesShift !! sideNum)
    sideNum = fieldId field `div` 10
    housePic = housePicture appState
    hotelPic = hotelPicture appState

drawUpgrades :: (Picture, Picture) -> Int -> Picture
drawUpgrades (housePic, hotelPic) num = case num of
  0 -> Blank
  5 -> Translate 25 0 hotelPic
  n -> Pictures (drawHouses housePic n)

drawHouses :: Picture -> Int -> [Picture]
drawHouses housePic 0 = []
drawHouses housePic idx = 
  [Translate (houseShift * i) 0 housePic] ++ drawHouses housePic (idx - 1)
  where 
    i = fromIntegral (idx - 1) :: Float

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
      (Pictures 
      (map (\ plr -> drawPlayerStats appState plr) (players appState)))
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
drawPlayerStats :: AppState -> PlayerState -> Picture
drawPlayerStats appState player = 
  Translate x y statText
  where
    (x, y)      = playersStatsShift !! id
    statText    = Color clr (Text statStr)
    statStr     = 
      arrow ++
      "Player" ++ idStr ++ ": " ++ balanceStr ++ " Status: " ++ statusStr
    idStr       = show (id + 1)
    balanceStr  = show (balance player)
    statusStr   = show (status player)
    clr         = playersColors !! id
    id          = playerId player
    arrow       = 
      if currentPlayerId appState == playerId player
      then 
        "->"
      else 
        ""

-- Handle events
handleEvent :: Event -> AppState -> AppState
-- Make turn when Space is pressed
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) appState = 
  startTurn appState
-- Ready / Agree to pay
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) appState = 
  payMoney appState
-- Agree to buy property by pressing 'y'
handleEvent (EventKey (Char 'y') Down _ _) appState =
  buyProperty appState True
-- Disagree to buy property by pressing 'n'
handleEvent (EventKey (Char 'n') Down _ _) appState =
  buyProperty appState False
-- Upgrade property by clicking LMB on it
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) appState = 
  case vec2FieldId (x, y) of 
    Just fieldId -> makeUpgrade appState fieldId
    Nothing -> appState
-- Mortgage property or sell upgrade by clicking RMB on it
handleEvent (EventKey (MouseButton RightButton) Down _ (x, y)) appState = 
  case vec2FieldId (x, y) of 
    Just fieldId -> makeDowngrade appState fieldId
    Nothing -> appState
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
  housePic <- loadBMP houseImagePath
  hotelPic <- loadBMP hotelImagePath
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
    , randomGens = (rndGen1, rndGen2) 
    , players = initiatedPlayers
    , boardPicture = boardPic
    , housePicture = housePic
    , hotelPicture = hotelPic
    , dicesPictures = dicesPics
    }

  play display bgColor fps initiatedAppState drawApp handleEvent updateApp
