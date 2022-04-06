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
bgColor = white

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

-- Shift of the board's center from the origin
boardCenterShift :: Vec
boardCenterShift = (-1024, 0)

-- Shift of the board's top left corner from it's center
boardTLCShift :: Vec
boardTLCShift = (-512, 512)

-- Shift of the players figure from field's top left corner
playerFieldShift :: Vec
playerFieldShift = (32, -32)

-- Shift of jailed player figure from field's top left corner
jailedShift :: Vec 
jailedShift = (32, 0)

-- Dices shift from each other
dicesShift :: Vec
dicesShift = (64, 0)

-- Fields coordinates relative to top left board corner
fieldCoords :: [Vec]
fieldCoords =
  [ (887.0, -887.0) -- 0
  , (804.0, -887.0) -- 1
  , (721.0, -887.0) -- 2
  , (638.0, -887.0) -- 3
  , (555.0, -887.0) -- 4
  , (471.0, -887.0) -- 5
  , (388.0, -887.0) -- 6
  , (305.0, -887.0) -- 7
  , (222.0, -887.0) -- 8
  , (138.0, -887.0) -- 9
  , (2.0, -887.0)   -- 10
  , (2.0, -804.0)   -- 11
  , (2.0, -721.0)   -- 12
  , (2.0, -638.0)   -- 13
  , (2.0, -555.0)   -- 14
  , (2.0, -471.0)   -- 15
  , (2.0, -388.0)   -- 16
  , (2.0, -305.0)   -- 17
  , (2.0, -222.0)   -- 18
  , (2.0, -138.0)   -- 19
  , (2.0, -2.0)     -- 20
  , (138.0, -2.0)   -- 21
  , (222.0, -2.0)   -- 22
  , (305.0, -2.0)   -- 23
  , (388.0, -2.0)   -- 24
  , (471.0, -2.0)   -- 25
  , (555.0, -2.0)   -- 26
  , (638.0, -2.0)   -- 27
  , (721.0, -2.0)   -- 28
  , (804.0, -2.0)   -- 29
  , (887.0, -2.0)   -- 30
  , (887.0, -138.0) -- 31
  , (887.0, -222.0) -- 32
  , (887.0, -305.0) -- 33
  , (887.0, -388.0) -- 34
  , (887.0, -471.0) -- 35
  , (887.0, -555.0) -- 36
  , (887.0, -638.0) -- 37
  , (887.0, -721.0) -- 38
  , (887.0, -804.0) -- 39
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
  AppState 0 (mkStdGen 0, mkStdGen 0) (0, 0) 0 0 [] initialBoardFields Blank [Blank]

-- Initial players state
initialPlayer0 :: PlayerState
initialPlayer0 = PlayerState 0 0 1500 Playing [] Blank

initialPlayer1 :: PlayerState
initialPlayer1 = PlayerState 1 0 1500 Playing [] Blank

initialPlayer2 :: PlayerState
initialPlayer2 = PlayerState 2 0 1500 Bankrupt [] Blank

initialPlayer3 :: PlayerState
initialPlayer3 = PlayerState 3 0 1500 Bankrupt [] Blank

initialPlayer4 :: PlayerState
initialPlayer4 = PlayerState 4 0 1500 Bankrupt [] Blank

-- Id of the jail field
jailFieldId :: Int
jailFieldId = 10

-- Initial board state
initialBoardFields :: [BoardField]
initialBoardFields = [
    BoardField 0 0 Start,
    BoardField 1 0 (Property (
      PropertyField (-1) 60 False 2 (Street (
        StreetField "brown" 0
      ))
    )),
    BoardField 2 0 CommunityChest,
    BoardField 3 0 (Property (
      PropertyField (-1) 60 False 4 (Street (
        StreetField "brown" 0
      ))
    )),
    BoardField 4 0 (Tax 200),
    BoardField 5 0 (Property (
      PropertyField (-1) 200 False 25 (RailwayStation)
    )),
    BoardField 6 0 (Property (
      PropertyField (-1) 100 False 6 (Street (
        StreetField "cyan" 0
      ))
    )),
    BoardField 7 0 Chance,
    BoardField 8 0 (Property (
      PropertyField (-1) 100 False 6 (Street (
        StreetField "cyan" 0
      ))
    )),
    BoardField 9 0 (Property (
      PropertyField (-1) 100 False 8 (Street (
        StreetField "cyan" 0
      ))
    )),
    BoardField 10 0 Jail,
    BoardField 11 0 (Property (
      PropertyField (-1) 140 False 10 (Street (
        StreetField "pink" 0
      ))
    )),
    BoardField 12 0 (Property (
      PropertyField (-1) 150 False 1 Utility
    )),
    BoardField 13 0 (Property (
      PropertyField (-1) 140 False 10 (Street (
        StreetField "pink" 0
      ))
    )),
    BoardField 14 0 (Property (
      PropertyField (-1) 140 False 12 (Street (
        StreetField "pink" 0
      ))
    )),
    BoardField 15 0 (Property (
      PropertyField (-1) 200 False 25 RailwayStation
    )),
    BoardField 16 0 (Property (
      PropertyField (-1) 180 False 14 (Street (
        StreetField "orange" 0
      ))
    )),
    BoardField 17 0 CommunityChest,
    BoardField 18 0 (Property (
      PropertyField (-1) 180 False 14 (Street (
        StreetField "orange" 0
      ))
    )),
    BoardField 19 0 (Property (
      PropertyField (-1) 180 False 16 (Street (
        StreetField "orange" 0
      ))
    )),
    BoardField 20 0 Parking,
    BoardField 21 0 (Property (
      PropertyField (-1) 220 False 18 (Street (
        StreetField "red" 0
      ))
    )),
    BoardField 22 0 Chance,
    BoardField 23 0 (Property (
      PropertyField (-1) 220 False 18 (Street (
        StreetField "red" 0
      ))
    )),
    BoardField 24 0 (Property (
      PropertyField (-1) 220 False 20 (Street (
        StreetField "red" 0
      ))
    )),
    BoardField 25 0 (Property (
      PropertyField (-1) 200 False 25 RailwayStation
    )),
    BoardField 26 0 (Property (
      PropertyField (-1) 260 False 22 (Street (
        StreetField "yellow" 0
      ))
    )),
    BoardField 27 0 (Property (
      PropertyField (-1) 260 False 22 (Street (
        StreetField "yellow" 0
      ))
    )),
    BoardField 28 0 (Property (
      PropertyField (-1) 150 False 1 Utility
    )),
    BoardField 29 0 (Property (
      PropertyField (-1) 260 False 24 (Street (
        StreetField "yellow" 0
      ))
    )),
    BoardField 30 0 Policeman,
    BoardField 31 0 (Property (
      PropertyField (-1) 300 False 26 (Street (
        StreetField "green" 0
      ))
    )),
    BoardField 32 0 (Property (
      PropertyField (-1) 300 False 26 (Street (
        StreetField "green" 0
      ))
    )),
    BoardField 33 0 CommunityChest,
    BoardField 34 0 (Property (
      PropertyField (-1) 300 False 28 (Street (
        StreetField "green" 0
      ))
    )),
    BoardField 35 0 (Property (
      PropertyField (-1) 200 False 25 RailwayStation
    )),
    BoardField 36 0 Chance,
    BoardField 37 0 (Property (
      PropertyField (-1) 350 False 35 (Street (
        StreetField "blue" 0
      ))
    )),
    BoardField 38 0 (Tax 100),
    BoardField 39 0 (Property 
      (PropertyField (-1) 400 False 50 (Street 
        (StreetField "blue" 0
      ))
    ))
  ] 