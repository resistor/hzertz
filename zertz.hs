module Zertz where

import qualified Data.Map as Map

data HexState = Empty | Open | White | Gray | Black
  deriving (Eq, Show)

type Coord = (Int, Int)
type ZertzBoard = Map.Map Coord HexState

type Score = (Int, Int, Int)

data ZertzState = ZertzState Score Score ZertzBoard Int
  deriving (Eq, Show)

getHex :: Coord -> ZertzBoard -> HexState
getHex = Map.findWithDefault Empty

blankBoard :: ZertzBoard
blankBoard = Map.fromList [((0,0), Open), ((1,0), Open), ((2,0), Open),
                           ((0,-1), Open), ((1,-1), Open), ((1,1), Open),
                           ((2,1), Open)]

nwHex :: Coord -> Coord
nwHex (x, y) = (x, y-1)
neHex :: Coord -> Coord
neHex (x, y) = (x+1, y)
eHex :: Coord -> Coord
eHex (x, y) = (x+1, y+1)
seHex :: Coord -> Coord
seHex (x, y) = (x, y+1)
swHex :: Coord -> Coord
swHex (x, y) = (x-1, y)
wHex :: Coord -> Coord
wHex (x, y) = (x-1, y-1)

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
      newBoard = removeMarble oldCoords board

placeMarble :: Coord -> HexState -> ZertzBoard -> ZertzBoard
placeMarble = Map.insert

removeMarble :: Coord -> ZertzBoard -> ZertzBoard
removeMarble = Map.delete

jumpMarble :: Coord -> Coord -> Coord -> ZertzBoard -> ZertzBoard
jumpMarble s m e = moveMarble s e . removeMarble m

scoreMarble :: Score -> HexState -> Score
scoreMarble (x, y, z) White = (x+1, y, z)
scoreMarble (x, y, z) Gray = (x, y+1, z)
scoreMarble (x, y, z) Black = (x, y, z+1)