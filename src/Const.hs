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

-- Board image file path
boardImagePath :: FilePath 
boardImagePath = "./resource/monopoly-board32v2.bmp"


-- Initial app state
initialAppState :: AppState
initialAppState = AppState 1 (0, 0) [initialPlayer0] initialBoard Blank

-- Initial player state
initialPlayer0 :: PlayerState
initialPlayer0 = PlayerState 0 0 1500 False []

-- Initial board state
initialBoard :: BoardState
initialBoard = BoardState 
  [
    BoardField 0 0 Start,
    BoardField 1 0 (Property (
      PropertyField (-1) 60 False 0 (Street (
        StreetField "brown" 0
      ))
    )),
    BoardField 2 0 CommunityChest,
    BoardField 3 0 (Property (
      PropertyField (-1) 60 False 0 (Street (
        StreetField "brown" 0
      ))
    )),
    BoardField 4 0 (Tax 200),
    BoardField 5 0 (Property (
      PropertyField (-1) 200 False 0 (RailwayStation)
    )),
    BoardField 6 0 (Property (
      PropertyField (-1) 100 False 0 (Street (
        StreetField "cyan" 0
      ))
    )),
    BoardField 7 0 Chance,
    BoardField 8 0 (Property (
      PropertyField (-1) 100 False 0 (Street (
        StreetField "cyan" 0
      ))
    )),
    BoardField 9 0 (Property (
      PropertyField (-1) 100 False 0 (Street (
        StreetField "cyan" 0
      ))
    )),
    BoardField 10 0 Jail,
    BoardField 11 0 (Property (
      PropertyField (-1) 140 False 0 (Street (
        StreetField "pink" 0
      ))
    )),
    BoardField 12 0 (Property (
      PropertyField (-1) 150 False 0 Utility
    )),
    BoardField 13 0 (Property (
      PropertyField (-1) 140 False 0 (Street (
        StreetField "pink" 0
      ))
    )),
    BoardField 14 0 (Property (
      PropertyField (-1) 140 False 0 (Street (
        StreetField "pink" 0
      ))
    )),
    BoardField 15 0 (Property (
      PropertyField (-1) 200 False 0 RailwayStation
    )),
    BoardField 16 0 (Property (
      PropertyField (-1) 180 False 0 (Street (
        StreetField "orange" 0
      ))
    )),
    BoardField 17 0 CommunityChest,
    BoardField 18 0 (Property (
      PropertyField (-1) 180 False 0 (Street (
        StreetField "orange" 0
      ))
    )),
    BoardField 19 0 (Property (
      PropertyField (-1) 180 False 0 (Street (
        StreetField "orange" 0
      ))
    )),
    BoardField 20 0 Parking,
    BoardField 21 0 (Property (
      PropertyField (-1) 220 False 0 (Street (
        StreetField "red" 0
      ))
    )),
    BoardField 22 0 Chance,
    BoardField 23 0 (Property (
      PropertyField (-1) 220 False 0 (Street (
        StreetField "red" 0
      ))
    )),
    BoardField 24 0 (Property (
      PropertyField (-1) 220 False 0 (Street (
        StreetField "red" 0
      ))
    )),
    BoardField 25 0 (Property (
      PropertyField (-1) 200 False 0 RailwayStation
    )),
    BoardField 26 0 (Property (
      PropertyField (-1) 260 False 0 (Street (
        StreetField "yellow" 0
      ))
    )),
    BoardField 27 0 (Property (
      PropertyField (-1) 260 False 0 (Street (
        StreetField "yellow" 0
      ))
    )),
    BoardField 28 0 (Property (
      PropertyField (-1) 150 False 0 Utility
    )),
    BoardField 29 0 (Property (
      PropertyField (-1) 260 False 0 (Street (
        StreetField "yellow" 0
      ))
    )),
    BoardField 30 0 Policeman,
    BoardField 31 0 (Property (
      PropertyField (-1) 300 False 0 (Street (
        StreetField "green" 0
      ))
    )),
    BoardField 32 0 (Property (
      PropertyField (-1) 300 False 0 (Street (
        StreetField "green" 0
      ))
    )),
    BoardField 33 0 CommunityChest,
    BoardField 34 0 (Property (
      PropertyField (-1) 300 False 0 (Street (
        StreetField "green" 0
      ))
    )),
    BoardField 35 0 (Property (
      PropertyField (-1) 200 False 0 RailwayStation
    )),
    BoardField 36 0 Chance,
    BoardField 37 0 (Property (
      PropertyField (-1) 350 False 0 (Street (
        StreetField "blue" 0
      ))
    )),
    BoardField 38 0 (Tax 100),
    BoardField 39 0 (Property 
      (PropertyField ((-1)) 400 False 0 (Street 
        (StreetField "blue" 0
      ))
    ))
  ] 