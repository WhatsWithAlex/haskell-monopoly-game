module Const where

import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.Environment

import Types 


-- Game display mode
display :: Display
display = FullScreen

-- Background color
bgColor :: Color
bgColor = black

-- Simulation steps per second
fps :: Int
fps = 60

-- Dice values images file paths
dice1ImagePath :: FilePath
dice1ImagePath = "./resource/dice1.bmp"

dice2ImagePath :: FilePath
dice2ImagePath = "./resource/dice2.bmp"

dice3ImagePath :: FilePath
dice3ImagePath = "./resource/dice3.bmp"

dice4ImagePath :: FilePath
dice4ImagePath = "./resource/dice4.bmp"

dice5ImagePath :: FilePath
dice5ImagePath = "./resource/dice5.bmp"

dice6ImagePath :: FilePath
dice6ImagePath = "./resource/dice6.bmp"

-- Players images file paths
player0ImagePath :: FilePath 
player0ImagePath = "./resource/player0.bmp"

player1ImagePath :: FilePath 
player1ImagePath = "./resource/player1.bmp"

player2ImagePath :: FilePath 
player2ImagePath = "./resource/player2.bmp"

player3ImagePath :: FilePath 
player3ImagePath = "./resource/player3.bmp"

player4ImagePath :: FilePath 
player4ImagePath = "./resource/player4.bmp"

-- Board image file path
boardImagePath :: FilePath 
boardImagePath = "./resource/monopoly-board32v5_1024.bmp"

-- House upgrade image file path
houseImagePath :: FilePath 
houseImagePath = "./resource/house.bmp"

-- House upgrade image file path
hotelImagePath :: FilePath 
hotelImagePath = "./resource/hotel.bmp"

-- Shift of the board's center from the origin
boardCenterShift :: Vec
boardCenterShift = (-1024.0, 0.0)

-- Shift of the board's top left corner from it's center
boardTLCShift :: Vec
boardTLCShift = (-512.0, 512.0)

-- Shift of the statistics' top left corner from the origin
statsShift :: Vec 
statsShift = (-512.0, 384.0)

-- Horizontal shift of the houses from each other
houseShift :: Float
houseShift = 16.0

-- Shift of the players figure from field's top left corner
playersFieldShift :: [Vec]
playersFieldShift = 
  [ (32.0, -16.0) -- 0
  , (32.0, -32.0) -- 1
  , (32.0, -48.0) -- 2
  , (32.0, -64.0) -- 3
  , (32.0, -80)   -- 4
  ]

-- Shift of the players stats from status window's top left corner
playersStatsShift :: [Vec]
playersStatsShift =
  [ (0.0, 0.0)    -- 0
  , (0.0, -128.0)  -- 1
  , (0.0, -256.0) -- 2
  , (0.0, -384.0) -- 3
  , (0.0, -512.0) -- 4
  ]

-- Shift of houses upgrades images from street's top left corner 
-- according to the side of the board
houseUpgradesShift :: [Vec]
houseUpgradesShift =
  [ (16.0, -90.0)  -- 0
  , (45.0, -16.0)  -- 1
  , (16.0, -90.0)  -- 2
  , (90.0, -64.0)     -- 3
  ]

-- Shift of jailed player figure from field's top left corner
jailedShift :: Vec 
jailedShift = (32, 0)

-- Dices shift from each other
dicesShift :: Vec
dicesShift = (64, 0)

-- Size of the corner board fields
cornerFieldSize :: Vec 
cornerFieldSize = (134.0, -134.0)

-- Size of the vertical board fields
vertFieldSize :: Vec 
vertFieldSize = (81.0, -134.0)

-- Size of the horizontal board fields
horFieldSize :: Vec 
horFieldSize = (134.0, -81.0)

-- Board field rectangles relative to the board's top left corner
fieldRects :: [Rectangle]
fieldRects =
  [ ((887.0, -887.0), cornerFieldSize)-- 0
  , ((804.0, -887.0), vertFieldSize)  -- 1
  , ((721.0, -887.0), vertFieldSize)  -- 2
  , ((638.0, -887.0), vertFieldSize)  -- 3
  , ((555.0, -887.0), vertFieldSize)  -- 4
  , ((471.0, -887.0), vertFieldSize)  -- 5
  , ((388.0, -887.0), vertFieldSize)  -- 6
  , ((305.0, -887.0), vertFieldSize)  -- 7
  , ((222.0, -887.0), vertFieldSize)  -- 8
  , ((138.0, -887.0), vertFieldSize)  -- 9
  , ((2.0, -887.0), cornerFieldSize)  -- 10
  , ((2.0, -804.0), horFieldSize)     -- 11
  , ((2.0, -721.0), horFieldSize)     -- 12
  , ((2.0, -638.0), horFieldSize)     -- 13
  , ((2.0, -555.0), horFieldSize)     -- 14
  , ((2.0, -471.0), horFieldSize)     -- 15
  , ((2.0, -388.0), horFieldSize)     -- 16
  , ((2.0, -305.0), horFieldSize)     -- 17
  , ((2.0, -222.0), horFieldSize)     -- 18
  , ((2.0, -138.0), horFieldSize)     -- 19
  , ((2.0, -2.0), cornerFieldSize)    -- 20
  , ((138.0, -2.0), vertFieldSize)    -- 21
  , ((222.0, -2.0), vertFieldSize)    -- 22
  , ((305.0, -2.0), vertFieldSize)    -- 23
  , ((388.0, -2.0), vertFieldSize)    -- 24
  , ((471.0, -2.0), vertFieldSize)    -- 25
  , ((555.0, -2.0), vertFieldSize)    -- 26
  , ((638.0, -2.0), vertFieldSize)    -- 27
  , ((721.0, -2.0), vertFieldSize)    -- 28
  , ((804.0, -2.0), vertFieldSize)    -- 29
  , ((887.0, -2.0), cornerFieldSize)  -- 30
  , ((887.0, -138.0), horFieldSize)   -- 31
  , ((887.0, -222.0), horFieldSize)   -- 32
  , ((887.0, -305.0), horFieldSize)   -- 33
  , ((887.0, -388.0), horFieldSize)   -- 34
  , ((887.0, -471.0), horFieldSize)   -- 35
  , ((887.0, -555.0), horFieldSize)   -- 36
  , ((887.0, -638.0), horFieldSize)   -- 37
  , ((887.0, -721.0), horFieldSize)   -- 38
  , ((887.0, -804.0), horFieldSize)   -- 39
  ]

-- Number of fields on the board
fieldNum :: Int
fieldNum = 40

-- Dices number range for random generator
diceNumRange :: (Int, Int)
diceNumRange = (1, 6)

-- Initial app state
initialAppState :: AppState
initialAppState = 
  AppState 0 (mkStdGen 0, mkStdGen 0) (0, 0) 0 0 [] initialBoardFields 
  Blank Blank Blank [Blank]

-- Initial players state
initialPlayer0 :: PlayerState
initialPlayer0 = PlayerState 0 0 1500 Playing [] Blank

initialPlayer1 :: PlayerState
initialPlayer1 = PlayerState 1 0 100 Playing [] Blank

initialPlayer2 :: PlayerState
initialPlayer2 = PlayerState 2 0 1500 Bankrupt [] Blank

initialPlayer3 :: PlayerState
initialPlayer3 = PlayerState 3 0 1500 Bankrupt [] Blank

initialPlayer4 :: PlayerState
initialPlayer4 = PlayerState 4 0 1500 Bankrupt [] Blank

-- Colors of the players
playersColors :: [Color]
playersColors = 
  [ red
  , yellow
  , green
  , blue
  , violet
  ]

-- Id of the jail field
jailFieldId :: Int
jailFieldId = 10

-- Max number of upgrades of the street
maxUpgrades :: Int
maxUpgrades = 5

-- Salary given to the player passing Start field
roundSalary :: Int
roundSalary = 200

-- Money the jailed player must pay to be released
releaseTax :: Int
releaseTax = 50

-- Initial board state
initialBoardFields :: [BoardField]
initialBoardFields = [
    BoardField 0 0 Start,
    BoardField 1 0 (Property (
      PropertyField (-1) 60 False 2 (Street (
        StreetField Brown 0
      ))
    )),
    BoardField 2 0 CommunityChest,
    BoardField 3 0 (Property (
      PropertyField (-1) 60 False 4 (Street (
        StreetField Brown 0
      ))
    )),
    BoardField 4 0 (Tax 200),
    BoardField 5 0 (Property (
      PropertyField (-1) 200 False 25 (RailwayStation)
    )),
    BoardField 6 0 (Property (
      PropertyField (-1) 100 False 6 (Street (
        StreetField LightBlue 0
      ))
    )),
    BoardField 7 0 Chance,
    BoardField 8 0 (Property (
      PropertyField (-1) 100 False 6 (Street (
        StreetField LightBlue 0
      ))
    )),
    BoardField 9 0 (Property (
      PropertyField (-1) 100 False 8 (Street (
        StreetField LightBlue 0
      ))
    )),
    BoardField 10 0 Jail,
    BoardField 11 0 (Property (
      PropertyField (-1) 140 False 10 (Street (
        StreetField Pink 0
      ))
    )),
    BoardField 12 0 (Property (
      PropertyField (-1) 150 False 1 Utility
    )),
    BoardField 13 0 (Property (
      PropertyField (-1) 140 False 10 (Street (
        StreetField Pink 0
      ))
    )),
    BoardField 14 0 (Property (
      PropertyField (-1) 140 False 12 (Street (
        StreetField Pink 0
      ))
    )),
    BoardField 15 0 (Property (
      PropertyField (-1) 200 False 25 RailwayStation
    )),
    BoardField 16 0 (Property (
      PropertyField (-1) 180 False 14 (Street (
        StreetField Orange 0
      ))
    )),
    BoardField 17 0 CommunityChest,
    BoardField 18 0 (Property (
      PropertyField (-1) 180 False 14 (Street (
        StreetField Orange 0
      ))
    )),
    BoardField 19 0 (Property (
      PropertyField (-1) 180 False 16 (Street (
        StreetField Orange 0
      ))
    )),
    BoardField 20 0 Parking,
    BoardField 21 0 (Property (
      PropertyField (-1) 220 False 18 (Street (
        StreetField Red 0
      ))
    )),
    BoardField 22 0 Chance,
    BoardField 23 0 (Property (
      PropertyField (-1) 220 False 18 (Street (
        StreetField Red 0
      ))
    )),
    BoardField 24 0 (Property (
      PropertyField (-1) 220 False 20 (Street (
        StreetField Red 0
      ))
    )),
    BoardField 25 0 (Property (
      PropertyField (-1) 200 False 25 RailwayStation
    )),
    BoardField 26 0 (Property (
      PropertyField (-1) 260 False 22 (Street (
        StreetField Yellow 0
      ))
    )),
    BoardField 27 0 (Property (
      PropertyField (-1) 260 False 22 (Street (
        StreetField Yellow 0
      ))
    )),
    BoardField 28 0 (Property (
      PropertyField (-1) 150 False 1 Utility
    )),
    BoardField 29 0 (Property (
      PropertyField (-1) 260 False 24 (Street (
        StreetField Yellow 0
      ))
    )),
    BoardField 30 0 Policeman,
    BoardField 31 0 (Property (
      PropertyField (-1) 300 False 26 (Street (
        StreetField Green 0
      ))
    )),
    BoardField 32 0 (Property (
      PropertyField (-1) 300 False 26 (Street (
        StreetField Green 0
      ))
    )),
    BoardField 33 0 CommunityChest,
    BoardField 34 0 (Property (
      PropertyField (-1) 300 False 28 (Street (
        StreetField Green 0
      ))
    )),
    BoardField 35 0 (Property (
      PropertyField (-1) 200 False 25 RailwayStation
    )),
    BoardField 36 0 Chance,
    BoardField 37 0 (Property (
      PropertyField (-1) 350 False 35 (Street (
        StreetField DarkBlue 0
      ))
    )),
    BoardField 38 0 (Tax 100),
    BoardField 39 0 (Property 
      (PropertyField (-1) 400 False 50 (Street 
        (StreetField DarkBlue 0
      ))
    ))
  ] 