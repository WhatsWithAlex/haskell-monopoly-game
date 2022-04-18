module Helpers where 

import Data.List ( findIndex )

import Types
import Const


-- Modify element of the list by index
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt _ _ [] = []
modifyAt n f (x : xs)
   | n == 0 = f x : xs
   | otherwise = x : modifyAt (n - 1) f xs

-- Replace element of the list by index
replaceAt :: Int -> a -> [a] -> [a]
replaceAt n newVal = modifyAt n (const newVal)

-- Remove given item from the list
removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y:ys) 
  | x == y    = removeItem x ys
  | otherwise = y : removeItem x ys

-- Sum of two vectors
(|+|) :: Vec -> Vec -> Vec 
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Difference of two vectors
(|-|) :: Vec -> Vec -> Vec 
(|-|) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- Multiply vector by scalar value
(|*|) :: Vec -> Float -> Vec
(|*|) (x, y) a = (a * x, a * y)

-- Get vector length
getVecLen :: Vec -> Float
getVecLen (x, y) = sqrt (x * x + y * y)

-- Normalize vector
normalizeVec :: Vec -> Vec
normalizeVec (x, y) = case len of 
  0.0 -> (0, 0)
  l   -> (x / l, y / l)
  where len = getVecLen (x, y)

-- Check if vector (point) is inside a rectangle
isInside :: Vec -> Rectangle -> Bool
isInside (x, y) (offset, size) = 
  x >= x1 && x <= x2 && (-y) >= (-y1) && (-y) <= (-y2)
  where
    x1 = fst offset
    x2 = x1 + fst size
    y1 = snd offset
    y2 = y1 + snd size

-- Get field id by position on screen
vec2FieldId :: Vec -> Maybe Int
vec2FieldId vec = 
  findIndex (isInside (xBrd, yBrd)) fieldRects
  where 
    (xBrd, yBrd) = vec |-| boardCenterShift |-| boardTLCShift

-- Get top left corner position of field on screen by field id
fieldId2Vec :: Int -> Vec
fieldId2Vec plrId =
  boardCenterShift |+|
  boardTLCShift |+| 
  fst (fieldRects !! plrId)

-- Get player info by id
getPlayer :: AppState -> Int -> PlayerState
getPlayer appState plrId = players appState !! plrId

-- Return current player info
getCurrentPlayer :: AppState -> PlayerState
getCurrentPlayer appState = getPlayer appState (currentPlayerId appState)

-- Evaluate new current player's position according to dices values
getNewCurrentPos :: AppState -> Int
getNewCurrentPos appState = (position curPlayer + diceSum) `mod` fieldNum 
  where
    diceSum   = fst (dicesValue appState) + snd (dicesValue appState)
    curPlayer = getCurrentPlayer appState

-- Change the position of current player
moveCurrentPlayer :: AppState -> Int -> AppState
moveCurrentPlayer appState pos = appState { players = updatedPlayers }
  where
    updatedPlayers = 
      modifyAt 
      (currentPlayerId appState) 
      (\ plr -> plr { position = pos }) 
      (players appState)

-- Change status of the current player
setCurPlayerStatus :: AppState -> Status -> AppState
setCurPlayerStatus appState newStatus = appState { players = updatedPlayers }
  where
    updatedPlayers = 
      modifyAt 
      (currentPlayerId appState) 
      (\ plr -> plr { status = newStatus }) 
      (players appState)

-- Decrease player's money
decreasePlayerBalance :: AppState -> Int -> Int -> AppState
decreasePlayerBalance appState plrId amount = 
  appState { players = updatedPlayers }
  where
    updatedPlayers  = 
      modifyAt 
      plrId (\ plr -> plr { balance = updBalance }) (players appState)
    updBalance = balance (getPlayer appState plrId) - amount

-- Decrease current player's money
decreaseCurPlayerBalance :: AppState -> Int -> AppState
decreaseCurPlayerBalance appState =
  decreasePlayerBalance appState (currentPlayerId appState)

-- Increase player's money
increasePlayerBalance :: AppState -> Int -> Int -> AppState
increasePlayerBalance appState plrId amount = 
  appState { players = updatedPlayers }
  where
    updatedPlayers  = 
      modifyAt 
      plrId (\ plr -> plr { balance = updBalance }) (players appState)
    updBalance = balance (getPlayer appState plrId) + amount

-- Increase current player's money
increaseCurPlayerBalance :: AppState -> Int -> AppState
increaseCurPlayerBalance appState =
  increasePlayerBalance appState (currentPlayerId appState)

-- Pay money to given player by current player
payToPlayer :: AppState -> Int -> Int -> AppState
payToPlayer appState plrId amount = 
  increasePlayerBalance decreasedState plrId amount
  where
    decreasedState = decreaseCurPlayerBalance appState amount

-- Get field information by field id
getBoardField :: AppState -> Int -> BoardField
getBoardField appState fldId = boardFields !! fldId
  where
    boardFields = fields appState

-- Get field type by field id
getFieldType :: AppState -> Int -> FieldType
getFieldType appState fldId = fieldType (getBoardField appState fldId)

-- Get information about property by field id
getProperty :: AppState -> Int -> PropertyField 
getProperty appState fldId = 
  case getFieldType appState fldId of
    Property prop -> prop
    _ -> error "getProperty: not a property"

-- Get property type by field id
getPropertyType :: AppState -> Int -> PropertyType 
getPropertyType appState fldId = propertyType (getProperty appState fldId)

-- Get information about street field by field id
getStreet :: AppState -> Int -> StreetField
getStreet appState fldId = 
  case getPropertyType appState fldId of
    Street street -> street
    _ -> error "getProperty: not a street"

-- Check if the property has owner
hasOwner :: PropertyField -> Bool 
hasOwner propertyField = ownerId propertyField /= -1

-- Add property field by id to the given player
addPlayerProperty :: AppState -> Int -> Int -> AppState
addPlayerProperty appState plrId fldId = 
  appState { players = updatedPlayers, fields = updFields }
  where
    updatedPlayers = 
      modifyAt 
      plrId
      (\ plr -> plr { ownProperty = updOwnProperty }) 
      (players appState)
    updOwnProperty = ownProperty player ++ [fldId]
    updFields = 
      modifyAt 
      fldId 
      (\ field -> field { fieldType = updProperty }) 
      (fields appState)
    updProperty   = Property propertyField { ownerId = plrId }
    propertyField = getProperty appState fldId
    player        = getPlayer appState plrId

-- Add property field by id to the current player
addCurPlayerProperty :: AppState -> Int -> AppState
addCurPlayerProperty appState = 
  addPlayerProperty appState (currentPlayerId appState)

-- Remove property field by id from the given player
removePlayerProperty :: AppState -> Int -> Int -> AppState
removePlayerProperty appState plrId fldId = 
  appState { players = updatedPlayers, fields = updFields }
  where
    updatedPlayers = 
      modifyAt 
      plrId
      (\ plr -> plr { ownProperty = updOwnProperty }) 
      (players appState)
    updOwnProperty = removeItem fldId (ownProperty player)
    updFields = 
      modifyAt 
      fldId 
      (\ field -> field { fieldType = updProperty }) 
      (fields appState)
    updProperty   = Property propertyField { ownerId = -1 }
    propertyField = getProperty appState fldId
    player        = getPlayer appState plrId

-- Remove property field by id from the current player
removeCurPlayerProperty :: AppState -> Int -> AppState
removeCurPlayerProperty appState = 
  removePlayerProperty appState (currentPlayerId appState)

-- Remove all property from the given player
removeCurPlayerAllProperty :: AppState -> AppState
removeCurPlayerAllProperty appState =  
  foldl removeCurPlayerProperty appState (ownProperty curPlayer)
  where 
    curPlayer = getCurrentPlayer appState

-- How many railway stations does player have
countOwnedStations :: AppState -> Int -> Int
countOwnedStations appState plrId = 
  length 
  (filter (\ fldId -> getPropertyType appState fldId == RailwayStation) 
  (ownProperty player))
  where
    player = getPlayer appState plrId

-- How many utilities does player have
countOwnedUtilities :: AppState -> Int -> Int
countOwnedUtilities appState plrId = 
  length 
  (filter (\ fldId -> getPropertyType appState fldId == Utility) 
  (ownProperty player))
  where
    player = getPlayer appState plrId

-- Calсulate the rent for the given property
getRentAmount :: AppState -> PropertyField -> Int
getRentAmount appState property = 
  case propertyType property of 
    Utility -> case countOwnedUtilities appState (ownerId property) of
      1 ->  4 * dicesSum
      2 -> 10 * dicesSum
      _ -> 0
    RailwayStation -> 
      initialRentPrice property * 
      countOwnedStations appState (ownerId property) 
    Street street -> case upgrades street of 
      0 -> initialRentPrice property
      x -> initialRentPrice property * 5 * x
    where 
      dicesSum = val1 + val2
      (val1, val2) = dicesValue appState

-- Sent current player to jail
jailCurPlayer :: AppState -> AppState
jailCurPlayer appState = 
  setCurPlayerStatus (moveCurrentPlayer appState jailFieldId) Jailed

-- Get mortgage price sum of all property owned by the given player
getPlayerPropertyPrice :: AppState -> Int -> Int
getPlayerPropertyPrice appState plrId = 
  sum $ map 
  (\ fldId -> case getPropertyType appState fldId of 
    Street str  -> 
      upgrades str * (getUpgradePrice appState fldId `div` 2)
    _ -> 
      buyPrice (getProperty appState fldId) `div` 2)
  (ownProperty player)
  where
    player = getPlayer appState plrId

-- Get mortgage price sum of all property owned by the current player
getCurPlayerPropertyPrice :: AppState -> Int
getCurPlayerPropertyPrice appState = 
  getPlayerPropertyPrice appState (currentPlayerId appState)

-- Check if player has enough property to pay
enoughCurProperty :: AppState -> Int -> Bool
enoughCurProperty appState amount = 
  getCurPlayerPropertyPrice appState >= amount

-- Check if current player has enough balance 
enoughCurBalance :: AppState -> Int -> Bool
enoughCurBalance appState amount = balance curPlayer >= amount
  where
    curPlayer = getCurrentPlayer appState

-- Check if current player has enough balance to pay and make payment
checkAndPay :: AppState -> Int -> Int -> AppState
checkAndPay appState plrId amount 
  | enoughCurBalance appState amount  = setCurPlayerStatus updState Playing
  | enoughCurProperty appState amount = appState
  | otherwise = 
    setCurPlayerStatus (removeCurPlayerAllProperty appState) Bankrupt
  where 
    updState = case plrId of 
      -1    -> decreaseCurPlayerBalance appState amount
      i -> payToPlayer appState i amount

-- Mortgage property by field id
mortgageProperty :: AppState -> Int -> AppState
mortgageProperty appState fldId = appState { fields = updFields }
  where
    updFields = 
      modifyAt 
      fldId 
      (\ field -> field { fieldType = updProperty }) 
      (fields appState)
    updProperty   = Property propertyField { turnsMortgaged = 1 }
    propertyField = getProperty appState fldId

-- Lift mortgaged property by field id
liftProperty :: AppState -> Int -> AppState
liftProperty appState fldId = appState { fields = updFields }
  where
    updFields = 
      modifyAt 
      fldId 
      (\ field -> field { fieldType = updProperty }) 
      (fields appState)
    updProperty   = Property propertyField { turnsMortgaged = 0 }
    propertyField = getProperty appState fldId

-- Calculate street field upgrade price
getUpgradePrice :: AppState -> Int -> Int
getUpgradePrice _ fldId = 50 * (fldId `div` 10 + 1)

-- Check if current player has all streets of the given color
hasMonopoly :: AppState -> StreetColor -> Bool
hasMonopoly appState clr = 
  if clr /= Brown && clr /= DarkBlue
  then
    length colorStreets == 3
  else
    length colorStreets == 2
  where 
    colorStreets = 
      filter 
      (\ fldId -> case getPropertyType appState fldId of 
        Street street -> streetColor street == clr
        _ -> False)
      (ownProperty curPlayer)
    curPlayer = getCurrentPlayer appState

-- Get minimum upgrade level of streets of the given color
getMinUpgrade :: AppState -> StreetColor -> Int
getMinUpgrade appState clr = 
  minimum $ map 
  (upgrades . getStreet appState . fieldId)
  colorStreets
  where
    colorStreets = 
      filter 
      (\ field -> case fieldType field of 
        Property propertyField -> case propertyType propertyField of 
          Street street -> streetColor street == clr
          _ -> False
        _ -> False)
      (fields appState)

-- Get maximum upgrade level of streets of the given color
getMaxUpgrade :: AppState -> StreetColor -> Int
getMaxUpgrade appState clr = 
  maximum $ map 
  (upgrades . getStreet appState . fieldId)
  colorStreets
  where
    colorStreets = 
      filter 
      (\ field -> case fieldType field of 
        Property propertyField -> case propertyType propertyField of 
          Street street -> streetColor street == clr
          _ -> False
        _ -> False)
      (fields appState)

-- Upgrade given street field
upgradeStreet :: AppState -> Int -> AppState
upgradeStreet appState fldId = appState { fields = updFields }
  where
    updFields = 
      modifyAt 
      fldId 
      (\ field -> field { fieldType = updProperty }) 
      (fields appState)
    updProperty   = Property propertyField { propertyType = updStreet }
    propertyField = getProperty appState fldId
    updStreet     = Street street { upgrades = upgrades street + 1 }
    street        = getStreet appState fldId

-- Downgrade given street field
downgradeStreet :: AppState -> Int -> AppState
downgradeStreet appState fldId = appState { fields = updFields }
  where
    updFields = 
      modifyAt 
      fldId 
      (\ field -> field { fieldType = updProperty }) 
      (fields appState)
    updProperty   = Property propertyField { propertyType = updStreet }
    propertyField = getProperty appState fldId
    updStreet     = Street street { upgrades = upgrades street - 1 }
    street        = getStreet appState fldId

-- Update current player's mortgaged property turn counter and
-- check if property is mortgaged more than 15 turns and
-- if true take it away from owner
processMortgagedProperty :: AppState -> AppState
processMortgagedProperty appState = newState
  where
    newState = foldl removeCurPlayerProperty updState removeProps
    updState = appState { fields = updFields }
    updFields =
      map 
      (\ field -> case fieldType field of 
        Property propertyField -> 
          if 
            turnsMortgaged propertyField > 0 && 
            ownerId propertyField == playerId curPlayer
          then
            field { fieldType = updFieldType }
          else
            field
          where 
            updFieldType = 
              if turnsMortgaged propertyField + 1 <= maxMortgagedTurns
              then
                Property 
                propertyField 
                { turnsMortgaged = turnsMortgaged propertyField + 1 }
              else
                Property 
                propertyField 
                { turnsMortgaged = 0 }
        _ -> field)
      (fields appState)
    removeProps = 
      filter 
      (\ fldId -> case getFieldType appState fldId of 
        Property propertyField -> turnsMortgaged propertyField + 1 > 15
        _ -> False)
      (ownProperty curPlayer)
    curPlayer = getCurrentPlayer appState

-- Update player figure position on screen
updatePlayerPos :: Float -> PlayerState -> PlayerState
updatePlayerPos t plr  = plr { picPos = updPos }
  where
    updPos  = if getVecLen (destPos |-| newPos) < 25
              then destPos
              else newPos
    newPos  = curPos |+| (dirVec |*| (t * figureAnimationVelocity))
    dirVec  = case normalizeVec (destPos |-| curPos) of
      (x, 0) -> (x, 0)
      (0, y) -> (0, y)
      (x, y) -> 
        if x > 0 && y > 0 
        then (0, 1)
        else if x > 0 && y < 0
        then (1, 0)
        else if x < 0 && y > 0
        then (-1, 0)
        else (0, -1)
    curPos  = picPos plr
    destPos = fieldId2Vec (position plr) 