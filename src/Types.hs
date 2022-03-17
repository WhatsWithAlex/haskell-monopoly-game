module Types where

import Graphics.Gloss.Interface.Pure.Game


-- General application state
data AppState = AppState
  { playerNumber :: Int         -- Number of current players
  , dicesValue :: (Int, Int)    -- Current dices value
  , players :: [PlayerState]    -- List of current players
  , board :: BoardState         -- Current board state
  , boardPicture :: Picture     -- Loaded board picture
  }
  
-- All infrormation about player
data PlayerState = PlayerState 
  { playerId :: Int         -- Player id number (0 - 4)
  , position :: Int         -- Position on board (BoardField id)
  , balance :: Int          -- Money the player have
  , isJailed :: Bool        -- True if the player is in jail
  , ownProperty :: [Int]    -- List with fields id's of property the player own
  }

-- Game board state
data BoardState = BoardState
  { fields :: [BoardField]  -- All board Fields
  }

-- All information about Field
data BoardField = BoardField 
  { fieldId :: Int          -- Field id number (0 - 39)
  , playerCount :: Int      -- How many players are on this field 
  , fieldType :: FieldType  -- Type of the field
  }

-- All field types
data FieldType = 
  Start | Jail | Parking | Policeman | -- Corner fields
  Tax Int | Chance | CommunityChest |  -- Penalties and rewards fields
  Property PropertyField               -- Field the player can own

-- All information about Property Field
data PropertyField = PropertyField
  { ownerId :: Int          -- Owner player id number (-1 if no owner, 0 - 4)
  , buyPrice :: Int         -- Price for buying the property
  , isMortgaged :: Bool     -- True if the property is mortaged
  , initialRentPrice :: Int -- How much the player must pay to the owner of the
                            -- property (w/o multipliers)
  , propertyType :: PropertyType
  }

-- All property types
data PropertyType = Utility | RailwayStation | Street StreetField

-- All information about street property
data StreetField = StreetField
  { color :: String     -- Color of the street
  , houseCount :: Int   -- How many houses are built on street
  }