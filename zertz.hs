module Zertz where

-- Zertz Module
-- This module defines the basic data types used for representing a game of
-- Zertz.  This includes representing the board, player scores, hexes, etc.
-- Also includes are functions for manipulating the game state.

--------------------------------------------------------------------------------
-- Data definitions
--------------------------------------------------------------------------------
import qualified Data.Map as Map

data HexState = Empty | Open | White | Gray | Black
  deriving (Eq, Show, Read)

type Coord = (Int, Int)
type ZertzBoard = Map.Map Coord HexState

type Score = (Int, Int, Int)

data ZertzState = ZertzState Score Score ZertzBoard Int
  deriving (Eq, Show, Read)

  --------------------------------------------------------------------------------
-- Data accessors
--------------------------------------------------------------------------------

-- getHex - Fetch the state of a given coordinate
getHex :: Coord -> ZertzBoard -> HexState
getHex = Map.findWithDefault Empty

-- blankBoard - The default board with no marbles
blankBoard :: ZertzBoard
blankBoard = Map.fromList $ map (\x -> (x, Open))
               [      (4,0),(4,1),(4,2),
                   (3,0),(3,1),(3,2),(3,3),
                (2,0),(2,1),(2,2),(2,3),(2,4),
                   (1,1),(1,2),(1,3),(1,4),
                      (0,2),(0,3),(0,4)       ]

startState :: ZertzState
startState = ZertzState (0,0,0) (0,0,0) blankBoard (-1)

-- *Hex - Generate to the coordinate pair in the given direction from the
-- given coordinate pair.
nwHex :: Coord -> Coord
nwHex (x, y) = (x+1, y-1)
neHex :: Coord -> Coord
neHex (x, y) = (x+1, y)
eHex :: Coord -> Coord
eHex (x, y) = (x, y+1)
seHex :: Coord -> Coord
seHex (x, y) = (x-1, y+1)
swHex :: Coord -> Coord
swHex (x, y) = (x-1, y)
wHex :: Coord -> Coord
wHex (x, y) = (x, y-1)

hexOccupied :: ZertzBoard -> Coord -> Bool
hexOccupied board coords =
  case getHex coords board of
    Empty -> False
    Open -> False
    _ -> True

hexOpen :: ZertzBoard -> Coord -> Bool
hexOpen board coords =
  case getHex coords board of
    Open -> True
    _ -> False

moveMarble :: Coord -> Coord -> ZertzBoard -> ZertzBoard
moveMarble oldCoords newCoords board =
  placeMarble newCoords color newBoard
    where
      color = getHex oldCoords board
      newBoard = placeMarble oldCoords Open board

marblesAvailable :: ZertzState -> HexState -> Int
marblesAvailable (ZertzState (w1,_,_) (w2,_,_) b _) White =
  let numOnBoard = Map.size $ Map.filter (== White) b in
    5 - numOnBoard - w1 - w2
marblesAvailable (ZertzState (_,g1,_) (_,g2,_) b _) Gray =
  let numOnBoard = Map.size $ Map.filter (== Gray) b in
    7 - numOnBoard - g1 - g2
marblesAvailable (ZertzState (_,_,b1) (_,_,b2) b _) Black =
  let numOnBoard = Map.size $ Map.filter (== Black) b in
    7 - numOnBoard - b1 - b2

placeMarble :: Coord -> HexState -> ZertzBoard -> ZertzBoard
placeMarble = Map.insert

removeMarble :: Coord -> ZertzBoard -> ZertzBoard
removeMarble = Map.delete

jumpMarble :: Coord -> Coord -> Coord -> ZertzBoard -> ZertzBoard
jumpMarble s m e = moveMarble s e . placeMarble m Open

scoreMarble :: Score -> HexState -> Score
scoreMarble (x, y, z) White = (x+1, y, z)
scoreMarble (x, y, z) Gray = (x, y+1, z)
scoreMarble (x, y, z) Black = (x, y, z+1)
