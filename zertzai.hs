module ZertzAI(ZertzState) where

import Zertz
import qualified Data.List as List
import qualified Data.Map as Map
import qualified MiniMax as MiniMax

instance MiniMax.WorldState ZertzState where
  generateMoves state = 
    case generateJumps state of
      [] -> generatePlacements state
      a -> a
  isTerminal st@(ZertzState s1 s2 _ _) = isEndgame s1 || isEndgame s2
  score foo = 0

isEndgame :: Score -> Bool
isEndgame (3, _, _) = True
isEndgame (_, 4, _) = True
isEndgame (_, _, 5) = True
isEndgame (2, 2, 2) = True
isEndgame _ = False

occupiedCoords :: ZertzState -> [((Int, Int), HexState)]
occupiedCoords state@(ZertzState _ _ b _) =
    Map.toList $ Map.filter occupied b
  where
    occupied h = h /= Empty && h /= Open

generateJumpsIter :: [ZertzState] -> [ZertzState]
generateJumpsIter states = do
    ws <- states
    case children ws of
      [] -> return ws
      n@_ -> n
  where 
    children ws@(ZertzState s1 s2 b p) = do
      (c, color)  <- occupiedCoords ws
      (start, end) <- [(eHex, wHex), (wHex, eHex), (swHex, neHex),
                       (neHex, swHex), (nwHex, seHex), (seHex, nwHex)]
      if (hexOccupied b $ start c) && (hexOpen b $ end c)
        then case p of
          1 -> return $ ZertzState (scoreMarble s1 color) s2
                                   (jumpMarble (start c) c (end c) b) p
          (-1) -> return $ ZertzState s1 (scoreMarble s2 color)
                                      (jumpMarble (start c) c (end c) b) p
        else []

generateJumps :: ZertzState -> [ZertzState]
generateJumps state =
  fixedPoint generateJumpsIter $ [state]
  where
    lengthOne (x:[]) = True
    lengthOne _ = False
    fixedPoint f = head . head . dropWhile lengthOne . List.group . iterate f

generatePlacements :: ZertzState -> [ZertzState]
generatePlacements (ZertzState s1 s2 b p) = do
    coord <- openHexes
    remCoord <- filter (== coord) openHexes
    color <- [White, Gray, Black]
    return $ ZertzState s1 s2 (placeMarble coord color b) p
  where
    openHexes = Map.keys $ Map.filter (== Open) b
