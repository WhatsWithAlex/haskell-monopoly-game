module Game where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap ( loadBMP )
import System.Random ( newStdGen, Random(randomR) )

import Types
import Const
import Helpers
import Graphics

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
    updState = case fldType of 
      Property propertyField -> 
        if 
          turnsMortgaged propertyField > 0 ||
          ownerId propertyField == currentPlayerId appState
        then
          movedState
        else if hasOwner propertyField
        then
          setCurPlayerStatus movedState Paying
        else
          setCurPlayerStatus movedState Buying
      Policeman -> jailCurPlayer movedState
      Tax _     -> setCurPlayerStatus movedState Paying
      _         -> movedState
    movedState = case doublesInRow appState of
      3 -> jailCurPlayer appState
      _ -> 
        if newCurPos < position curPlayer
        then
          increaseCurPlayerBalance newState roundSalary
        else 
          newState
        where
          newState  = moveCurrentPlayer appState newCurPos
          newCurPos = getNewCurrentPos appState
          curPlayer = getCurrentPlayer appState
    fldType     = getFieldType movedState (position movedPlayer)
    movedPlayer = getCurrentPlayer movedState

-- Process buying property
buyProperty :: AppState -> Bool -> AppState
buyProperty appState isBuy
  | status curPlayer /= Buying  = appState
  | isBuy                       = passNextTurn buyState
  | otherwise                   = passNextTurn aucState
  where   
    buyState  = case fldType of
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
    fldType   = getFieldType appState (position curPlayer)
    curPlayer = getCurrentPlayer appState
    aucState  = setCurPlayerStatus appState Playing
  
-- Process paying rent / tax / releasing from jail
payMoney :: AppState -> AppState
payMoney appState
  | status curPlayer == Paying  = passNextTurn payState
  | status curPlayer == Jailed  = payState
  | otherwise                   = appState
  where   
    payState  = case fldType of
      Property propertyField -> 
        checkAndPay appState (ownerId propertyField) rentAmount
        where
          rentAmount = getRentAmount appState propertyField
      Tax amount  -> checkAndPay appState (-1) amount
      Jail        -> checkAndPay appState (-1) releaseTax
      _           -> appState
    fldType   = getFieldType appState (position curPlayer)
    curPlayer = getCurrentPlayer appState

-- Process upgrading / lifting mortgaged property
makeUpgrade :: AppState -> Int -> AppState
makeUpgrade appState fldId = case getFieldType appState fldId of 
  Property propertyField -> case propertyType propertyField of 
    Street streetField -> 
      if 
        enoughCurBalance appState upgradePrice &&
        hasMonopoly appState (streetColor streetField) &&
        turnsMortgaged propertyField == 0 &&
        upgrades streetField == curMinUpgrade &&
        upgrades streetField < maxUpgrades
      then 
        upgradeStreet 
        (decreaseCurPlayerBalance appState upgradePrice)
        fldId
      else if 
        enoughCurBalance appState liftPrice &&
        ownrId == currentPlayerId appState &&
        turnsMortgaged propertyField > 0
      then
        liftProperty
        (decreaseCurPlayerBalance appState liftPrice)
        fldId
      else
        appState
      where 
        upgradePrice  = getUpgradePrice appState fldId
        liftPrice     = price `div` 2 + price `div` 10
        price      = buyPrice propertyField
        curMinUpgrade = getMinUpgrade appState (streetColor streetField)
        ownrId        = ownerId (getProperty appState fldId)
    _ ->
      if 
        enoughCurBalance appState liftPrice &&
        ownrId == currentPlayerId appState &&
        turnsMortgaged propertyField > 0
      then
        liftProperty
        (decreaseCurPlayerBalance appState liftPrice)
        fldId
      else
        appState
      where 
        liftPrice     = price `div` 2 + price `div` 10
        price         = buyPrice propertyField
        ownrId        = ownerId (getProperty appState fldId)
  _ -> appState

-- Process downgrading / mortgaging property
makeDowngrade :: AppState -> Int -> AppState
makeDowngrade appState fldId = case getFieldType appState fldId of 
  Property propertyField -> case propertyType propertyField of 
    Street streetField -> 
      if 
        ownrId == currentPlayerId appState &&
        upgrades streetField == maxUpgrade &&
        upgrades streetField > 0
      then 
        downgradeStreet 
        (increaseCurPlayerBalance appState downgradePayment)
        fldId
      else if 
        ownrId == currentPlayerId appState &&
        upgrades streetField == 0 &&
        turnsMortgaged propertyField == 0
      then
        mortgageProperty
        (increaseCurPlayerBalance appState mortgagePayment)
        fldId
      else 
        appState
      where
        maxUpgrade        = getMaxUpgrade appState (streetColor streetField)
        downgradePayment  = getUpgradePrice appState fldId `div` 2
        mortgagePayment   = buyPrice propertyField `div` 2
        ownrId            = ownerId (getProperty appState fldId)
    _ ->
      if 
        ownrId == currentPlayerId appState &&
        turnsMortgaged propertyField == 0
      then
        mortgageProperty
        (increaseCurPlayerBalance appState mortgagePayment)
        fldId
      else 
        appState
      where
        mortgagePayment   = buyPrice propertyField `div` 2
        ownrId            = ownerId (getProperty appState fldId)
  _ -> appState

-- Pass the turn to the next player in game
passNextTurn :: AppState -> AppState
passNextTurn appState
  | status curPlayer  == Jailed   = passNextTurn updState
  | status curPlayer  == Playing &&
    doublesInRow appState > 0     = appState
  | status curPlayer  == Buying   = appState
  | status curPlayer  == Paying   = appState
  | status nextPlayer == Bankrupt = passNextTurn updState
  | status nextPlayer == Playing  = updState
  | status nextPlayer == Jailed   = updState
  | otherwise                     = passNextTurn updState
  where 
    updState      = 
      processMortgagedProperty appState { currentPlayerId = nextPlayerId }
    curPlayer     = getCurrentPlayer appState
    nextPlayer    = players appState !! nextPlayerId
    nextPlayerId  = 
      (currentPlayerId appState + 1) `mod` playerNumber appState

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
    Just fldId -> makeUpgrade appState fldId
    Nothing -> appState
-- Mortgage property or sell upgrade by clicking RMB on it
handleEvent (EventKey (MouseButton RightButton) Down _ (x, y)) appState = 
  case vec2FieldId (x, y) of 
    Just fldId -> makeDowngrade appState fldId
    Nothing -> appState
-- Ignore all other events.
handleEvent _ appState = appState

-- Simulation step (animation)
updateApp :: Float -> AppState -> AppState
updateApp t appState = appState { players = updPlayers }
  where
    updPlayers = map (updatePlayerPos t) (players appState)

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

  let initPicPos = fieldId2Vec 0
  let initiatedPlayer0 = initialPlayer0 { 
      playerPicture = player0Pic 
    , picPos = initPicPos 
    }
  let initiatedPlayer1 = initialPlayer1 { 
      playerPicture = player1Pic 
    , picPos = initPicPos 
    }
  let initiatedPlayer2 = initialPlayer2 { 
      playerPicture = player2Pic 
    , picPos = initPicPos 
    }
  let initiatedPlayer3 = initialPlayer3 { 
      playerPicture = player3Pic 
    , picPos = initPicPos 
    }
  let initiatedPlayer4 = initialPlayer4 { 
      playerPicture = player4Pic 
    , picPos = initPicPos 
    }

  let dicesPics = [dice1Pic, dice2Pic, dice3Pic, dice4Pic, dice5Pic, dice6Pic]
  let initiatedAppState = initialAppState { 
      playerNumber = 5
    , randomGens = (rndGen1, rndGen2) 
    , players = 
      [ initiatedPlayer0
      , initiatedPlayer1 
      , initiatedPlayer2
      , initiatedPlayer3
      , initiatedPlayer4
      ]
    , boardPicture = boardPic
    , housePicture = housePic
    , hotelPicture = hotelPic
    , dicesPictures = dicesPics
    }

  play display bgColor fps initiatedAppState drawApp handleEvent updateApp
