module Types where

import Graphics.Gloss.Interface.Pure.Game ( Picture )
import System.Random ( StdGen )


-- 2D Point/Vector on screen (in pixels)
type Vec = (Float, Float)
-- Also treat float pair as size
type Size = Vec
-- Rectangle represented by top left corner coords and size
type Rectangle = (Vec, Size)

-- Button data type for GUI
data PressButton = PressButton 
  { buttonId :: Int
  , buttonName :: String
  , buttonPos :: (Int, Int)
  , buttonSize :: (Int, Int)
  , isActive :: Bool
  , isVisible :: Bool
  , buttonPic :: Picture
  }

-- General application state
data AppState = AppState
  { playerNumber :: Int           -- Number of current players
  , randomGens :: (StdGen, StdGen)-- Random generator for dices values
  , dicesValue :: (Int, Int)      -- Current dices value
  , doublesInRow :: Int           -- How many times in a row doubles are thrown
  , currentPlayerId :: Int        -- Id of the player to make a turn
  , players :: [PlayerState]      -- List of current players
  , fields :: [BoardField]        -- List of all fields
  , boardPicture :: Picture       -- Loaded board picture
  , housePicture :: Picture       -- Loaded house upgrade picture
  , hotelPicture :: Picture       -- Loaded hotel upgrade picture
  , dicesPictures :: [Picture]    -- Loaded dices pictures
  }
  deriving Show
  
-- All information about player
data PlayerState = PlayerState 
  { playerId :: Int           -- Player id number (0 - 4)
  , position :: Int           -- Position on board (BoardField id)
  , balance :: Int            -- Amount of player's money
  , status :: Status          -- Current player status
  , turnsInJail :: Int        -- How many turns player is in jail
  , ownProperty :: [Int]      -- List with fields id's of player's property
  , playerPicture :: Picture  -- Loaded player picture
  , picPos :: Vec             -- Position of the player picture on the screen
  }
  deriving Show

-- Possible player statuses
data Status = 
  Playing | Bankrupt | Jailed | Buying | Paying | Auctoring | Trading
  deriving (Show, Eq)

-- All information about Field
data BoardField = BoardField 
  { fieldId :: Int          -- Field id number (0 - 39)
  , playerCount :: Int      -- How many players are on this field 
  , fieldType :: FieldType  -- Type of the field
  }
  deriving Show

-- All field types
data FieldType = 
  Start | Jail | Parking | Policeman | -- Corner fields
  Tax Int | Chance | CommunityChest |  -- Penalties and rewards fields
  Property PropertyField               -- Field the player can own
  deriving (Show, Eq)

-- All information about Property Field
data PropertyField = PropertyField
  { ownerId :: Int          -- Owner player id number (-1 if no owner, 0 - 4)
  , buyPrice :: Int         -- Price for buying the property
  , turnsMortgaged :: Int   -- How many turns is property mortgaged
  , initialRentPrice :: Int -- How much the player must pay to the owner of the
                            -- property (w/o multipliers)
  , propertyType :: PropertyType
  }
  deriving (Show, Eq)

-- All property types
data PropertyType = Utility | RailwayStation | Street StreetField
  deriving (Show, Eq)
  
-- All information about street property
data StreetField = StreetField
  { streetColor :: StreetColor  -- Color of the street
  , upgrades :: Int       -- How many houses are built on street
  }
  deriving (Show, Eq)

-- All street colors
data StreetColor = 
  Brown | LightBlue | 
  Pink | Orange | 
  Red | Yellow | 
  Green | DarkBlue
  deriving (Show, Eq)