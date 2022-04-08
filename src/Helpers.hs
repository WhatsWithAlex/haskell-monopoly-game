module Helpers where 

import Data.List
import Debug.Trace

import Types
import Const


-- Modify element of the list by index
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt _ _ [] = []
modifyAt n f (x : xs)
   | n == 0 = (f x) : xs
   | otherwise = x : modifyAt (n - 1) f xs

-- Replace element of the list by index
replaceAt :: Int -> a -> [a] -> [a]
replaceAt n newVal = modifyAt n (\ x -> newVal)

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
  findIndex (\ rect -> isInside (xBrd, yBrd) rect) fieldRects
  where 
    (xBrd, yBrd) = vec |-| boardCenterShift |-| boardTLCShift

-- Get top left corner position of field on screen by field id
fieldId2Vec :: Int -> Vec
fieldId2Vec id =
  boardCenterShift |+|
  boardTLCShift |+| 
  fst (fieldRects !! id)

-- Get player info by id
getPlayer :: AppState -> Int -> PlayerState
getPlayer appState id = (players appState) !! id

-- Return current player info
getCurrentPlayer :: AppState -> PlayerState
getCurrentPlayer appState = getPlayer appState (currentPlayerId appState)

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
decreasePlayerBalance appState playerId amount = 
  appState { players = updatedPlayers }
  where
    updatedPlayers  = 
      modifyAt 
      playerId (\ plr -> plr { balance = updBalance }) (players appState)
    updBalance = (balance (getPlayer appState playerId)) - amount

-- Decrease current player's money
decreaseCurPlayerBalance :: AppState -> Int -> AppState
decreaseCurPlayerBalance appState amount =
  decreasePlayerBalance appState (currentPlayerId appState) amount

-- Increase player's money
increasePlayerBalance :: AppState -> Int -> Int -> AppState
increasePlayerBalance appState playerId amount = 
  appState { players = updatedPlayers }
  where
    updatedPlayers  = 
      modifyAt 
      playerId (\ plr -> plr { balance = updBalance }) (players appState)
    updBalance = (balance (getPlayer appState playerId)) + amount

-- Increase current player's money
increaseCurPlayerBalance :: AppState -> Int -> AppState
increaseCurPlayerBalance appState amount =
  increasePlayerBalance appState (currentPlayerId appState) amount

-- Pay money to given player by current player
payToPlayer :: AppState -> Int -> Int -> AppState
payToPlayer appState playerId amount = 
  increasePlayerBalance decreasedState playerId amount
  where
    decreasedState = decreaseCurPlayerBalance appState amount

-- Get field information by field id
getBoardField :: AppState -> Int -> BoardField
getBoardField appState id = boardFields !! id
  where
    boardFields = fields appState

-- Get field type by field id
getFieldType :: AppState -> Int -> FieldType
getFieldType appState id = fieldType (getBoardField appState id)

-- Get information about property by field id
getProperty :: AppState -> Int -> PropertyField 
getProperty appState id = 
  case getFieldType appState id of
    Property prop -> prop

-- Get property type by field id
getPropertyType :: AppState -> Int -> PropertyType 
getPropertyType appState id = propertyType (getProperty appState id)

-- Get information about street field by field id
getStreet :: AppState -> Int -> StreetField
getStreet appState id = 
  case getPropertyType appState id of
    Street street -> street

-- Check if the property has owner
hasOwner :: PropertyField -> Bool 
hasOwner propertyField = ownerId propertyField /= -1

-- Add property field by id to the given player
addPlayerProperty :: AppState -> Int -> Int -> AppState
addPlayerProperty appState playerId fieldId = 
  appState { players = updatedPlayers, fields = updFields }
  where
    updatedPlayers = 
      modifyAt 
      playerId
      (\ plr -> plr { ownProperty = updOwnProperty }) 
      (players appState)
    updOwnProperty = (ownProperty player) ++ [fieldId]
    updFields = 
      modifyAt 
      fieldId 
      (\ field -> field { fieldType = updProperty }) 
      (fields appState)
    updProperty   = Property propertyField { ownerId = playerId }
    propertyField = getProperty appState fieldId
    player        = getPlayer appState playerId

-- Add property field by id to the current player
addCurPlayerProperty :: AppState -> Int -> AppState
addCurPlayerProperty appState fieldId = 
  addPlayerProperty appState (currentPlayerId appState) fieldId

-- Remove property field by id from the given player
removePlayerProperty :: AppState -> Int -> Int -> AppState
removePlayerProperty appState playerId fieldId = 
  appState { players = updatedPlayers, fields = updFields }
  where
    updatedPlayers = 
      modifyAt 
      playerId
      (\ plr -> plr { ownProperty = updOwnProperty }) 
      (players appState)
    updOwnProperty = removeItem fieldId (ownProperty player)
    updFields = 
      modifyAt 
      fieldId 
      (\ field -> field { fieldType = updProperty }) 
      (fields appState)
    updProperty   = Property propertyField { ownerId = -1 }
    propertyField = getProperty appState fieldId
    player        = getPlayer appState playerId

-- Remove property field by id from the current player
removeCurPlayerProperty :: AppState -> Int -> AppState
removeCurPlayerProperty appState fieldId = 
  removePlayerProperty appState (currentPlayerId appState) fieldId

-- Remove all property from the given player
removeCurPlayerAllProperty :: AppState -> AppState
removeCurPlayerAllProperty appState =  
  foldl removeCurPlayerProperty appState (ownProperty curPlayer)
  where 
    curPlayer = getCurrentPlayer appState

-- How many railway stations does player have
countOwnedStations :: AppState -> Int -> Int
countOwnedStations appState playerId = 
  length 
  (filter (\ fieldId -> getPropertyType appState fieldId == RailwayStation) 
  (ownProperty player))
  where
    player = getPlayer appState playerId

-- How many utilities does player have
countOwnedUtilities :: AppState -> Int -> Int
countOwnedUtilities appState playerId = 
  length 
  (filter (\ fieldId -> getPropertyType appState fieldId == Utility) 
  (ownProperty player))
  where
    player = getPlayer appState playerId

-- Calсulate the rent for the given property
getRentAmount :: AppState -> PropertyField -> Int
getRentAmount appState property = 
  case propertyType property of 
    Utility -> case countOwnedUtilities appState (ownerId property) of
      1 ->  4 * dicesSum
      2 -> 10 * dicesSum
    RailwayStation -> 
      (initialRentPrice property) * 
      (countOwnedStations appState (ownerId property)) 
    Street street -> case upgrades street of 
      0 -> initialRentPrice property
      x -> (initialRentPrice property) * 5 * x
    where 
      dicesSum = val1 + val2
      (val1, val2) = dicesValue appState

-- Sent current player to jail
jailCurPlayer :: AppState -> AppState
jailCurPlayer appState = 
  setCurPlayerStatus (moveCurrentPlayer appState jailFieldId) Jailed

-- Get mortgage price sum of all property owned by the given player
getPlayerPropertyPrice :: AppState -> Int -> Int
getPlayerPropertyPrice appState playerId = 
  sum $ map 
  (\ fieldId -> case getPropertyType appState fieldId of 
    Street str  -> 
      (upgrades str) * ((getUpgradePrice appState fieldId) `div` 2)
    _ -> 
      buyPrice (getProperty appState fieldId) `div` 2)
  (ownProperty player)
  where
    player = getPlayer appState playerId

-- Get mortgage price sum of all property owned by the current player
getCurPlayerPropertyPrice :: AppState -> Int
getCurPlayerPropertyPrice appState = 
  getPlayerPropertyPrice appState (currentPlayerId appState)

-- Check if player has enough property to pay
enoughCurProperty :: AppState -> Int -> Bool
enoughCurProperty appState amount = 
  (getCurPlayerPropertyPrice appState) >= amount

-- Check if current player has enough balance 
enoughCurBalance :: AppState -> Int -> Bool
enoughCurBalance appState amount = (balance curPlayer) >= amount
  where
    curPlayer = getCurrentPlayer appState

-- Check if current player has enough balance to pay and make payment
checkAndPay :: AppState -> Int -> Int -> AppState
checkAndPay appState playerId amount = 
  if enoughCurBalance appState amount
  then 
    setCurPlayerStatus updState Playing
  else if enoughCurProperty appState amount
  then
    appState
  else
    setCurPlayerStatus (removeCurPlayerAllProperty appState) Bankrupt
  where 
    updState = case playerId of 
      -1 -> decreaseCurPlayerBalance appState amount
      id -> payToPlayer appState id amount

-- Mortgage property by field id
mortgageProperty :: AppState -> Int -> AppState
mortgageProperty appState fieldId = appState { fields = updFields }
  where
    updFields = 
      modifyAt 
      fieldId 
      (\ field -> field { fieldType = updProperty }) 
      (fields appState)
    updProperty   = Property propertyField { isMortgaged = True }
    propertyField = getProperty appState fieldId

-- Lift mortgaged property by field id
liftProperty :: AppState -> Int -> AppState
liftProperty appState fieldId = appState { fields = updFields }
  where
    updFields = 
      modifyAt 
      fieldId 
      (\ field -> field { fieldType = updProperty }) 
      (fields appState)
    updProperty   = Property propertyField { isMortgaged = False }
    propertyField = getProperty appState fieldId

-- Calculate street field upgrade price
getUpgradePrice :: AppState -> Int -> Int
getUpgradePrice appState fieldId = 50 * (fieldId `div` 10 + 1)

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
      (\ fieldId -> case getPropertyType appState fieldId of 
        Street street -> streetColor street == clr
        _ -> False)
      (ownProperty curPlayer)
    curPlayer = getCurrentPlayer appState

-- Get minimum upgrade level of streets of the given color
getMinUpgrade :: AppState -> StreetColor -> Int
getMinUpgrade appState clr = 
  minimum $ map 
  (\ fld -> upgrades (getStreet appState (fieldId fld)))
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
  (\ fld -> upgrades (getStreet appState (fieldId fld)))
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
upgradeStreet appState fieldId = appState { fields = updFields }
  where
    updFields = 
      modifyAt 
      fieldId 
      (\ field -> field { fieldType = updProperty }) 
      (fields appState)
    updProperty   = Property propertyField { propertyType = updStreet }
    propertyField = getProperty appState fieldId
    updStreet     = Street street { upgrades = (upgrades street) + 1 }
    street        = getStreet appState fieldId

-- Downgrade given street field
downgradeStreet :: AppState -> Int -> AppState
downgradeStreet appState fieldId = appState { fields = updFields }
  where
    updFields = 
      modifyAt 
      fieldId 
      (\ field -> field { fieldType = updProperty }) 
      (fields appState)
    updProperty   = Property propertyField { propertyType = updStreet }
    propertyField = getProperty appState fieldId
    updStreet     = Street street { upgrades = (upgrades street) - 1 }
    street        = getStreet appState fieldId

