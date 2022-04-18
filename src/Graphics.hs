module Graphics where

import Graphics.Gloss.Interface.Pure.Game

import Types 
import Const
import Helpers

-- Draw an app
drawApp :: AppState -> Picture
drawApp appState = 
  Pictures [boardPic, fieldsPic, playersPic, infoPic, upgradesPic]
  where 
    boardPic    = uncurry Translate boardCenterShift (boardPicture appState)
    fieldsPic   = Pictures (map drawField (fields appState))
    playersPic  = Pictures (map drawPlayer (players appState))
    infoPic     = drawInfo appState
    upgradesPic = 
      Pictures
      (map (drawFieldUpgrades appState) (fields appState))

-- Draw information about field on board
drawField :: BoardField -> Picture
drawField field = case fieldType field of
  Property propertyField -> 
    if ownerId propertyField /= -1 
    then 
      if turnsMortgaged propertyField > 0
      then 
        Pictures [Color clr (Pictures [fieldPic, mortgagedPic]), counterPic]
      else 
        Color clr fieldPic
    else
      Blank
    where
      fieldPic = Translate (x + w / 2 + 1) (y + h / 2) (rectangleWire w (-h))
      mortgagedPic = Line [(x, y), (x + w, y + h)]
      counterPic = 
        Translate (x + w / 3) (y + 0.75 * h) $
        Scale 0.35 0.35 $
        Text (show $ maxMortgagedTurns + 1 - turnsMortgaged propertyField)
      clr = playersColors !! ownerId propertyField
      (x, y) = fieldId2Vec (fieldId field)
      (w, h) = snd (fieldRects !! fieldId field)
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
drawHouses _ 0 = []
drawHouses housePic idx = 
  Translate (houseShift * i) 0 housePic : drawHouses housePic (idx - 1)
  where 
    i = fromIntegral (idx - 1) :: Float

-- Draw player figure on board
drawPlayer :: PlayerState -> Picture
drawPlayer player 
  | status player == Jailed    = Translate xJail yJail pic
  | status player /= Bankrupt  = Translate x y pic
  | otherwise                  = Blank
  where 
      (x, y)  = picPos player |+| offset
      (xJail, yJail) = fieldId2Vec jailFieldId |+| jailedShift |+| offset
      pic     = playerPicture player
      offset  = playersFieldShift !! playerId player

-- Draw game info and statistics
drawInfo :: AppState -> Picture
drawInfo appState = Pictures [dicesPic, playersStatsPic]
  where
    dicesPic  = Translate xDices yDices (drawDices (v1, v2) dicesPics)
    dicesPics = dicesPictures appState 
    (v1, v2)  = dicesValue appState
    playersStatsPic = 
      Translate xStats yStats $
      Scale 0.35 0.35 $
      Pictures 
      (map (drawPlayerStats appState) (players appState))
    (xDices, yDices) = boardCenterShift
    (xStats, yStats) = statsShift
    
-- Draw dices images
drawDices :: (Int, Int) -> [Picture] -> Picture
drawDices (0, 0) _      = Blank
drawDices (v1, v2) pics = Pictures [pic1, pic2]
  where
    pic1 = Translate (-x) (-y) (pics !! (v1 - 1))
    pic2 = Translate x y (pics !! (v2 - 1))
    (x, y) = dicesShift

-- Draw player balance and status info
drawPlayerStats :: AppState -> PlayerState -> Picture
drawPlayerStats appState player = Translate x y statText
  where
    (x, y)      = playersStatsShift !! plrId
    statText    = Text statStr
    statStr     = 
      arrow ++
      "Player" ++ idStr ++ ": " ++ balanceStr ++ " Status: " ++ statusStr
    idStr       = show (plrId + 1)
    balanceStr  = show (balance player)
    statusStr   = show (status player)
    plrId       = playerId player
    arrow       = 
      if currentPlayerId appState == playerId player
      then 
        "->"
      else 
        ""