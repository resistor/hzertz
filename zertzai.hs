module ZertzAI(ZertzState(..)) where

import Zertz
import qualified Data.List as List
import qualified Data.Map as Map
import qualified MiniMax as MiniMax

-- Provide instance implementations to make ZertzState valid input
-- to the minimax solver.
instance MiniMax.WorldState ZertzState where
  -- generateMoves - If there are jumps available, we must take one.  Otherwise,
  -- we perform a placement.
  generateMoves state 
    | null $ generateJumps state = generatePlacements state
    | otherwise                  = generateJumps state

  -- isTerminal - The game state is terminal if either player has a winning
  -- score set.
  isTerminal st@(ZertzState s1 s2 _ _) = isEndgame s1 || isEndgame s2
  
  -- score - The score function is defined as the number of balls player 2 needs
  -- to win minus the number of balls player 1 needs to win.  This makes the
  -- score zero-sum, which is needed for negamax.
  score (ZertzState s1 s2 _ _) = (marblesToWin s2) - (marblesToWin s1)

-- isEndgame - Simple predicate that determines if a given score set is a 
-- a winning combination.
isEndgame :: Score -> Bool
isEndgame (3, _, _) = True
isEndgame (_, 4, _) = True
isEndgame (_, _, 5) = True
isEndgame (2, 2, 2) = True
isEndgame _ = False

-- marblesToWin - Returns the number of marbles needed to turn a given hand into
-- a winning one.
marblesToWin :: Score -> Int
marblesToWin (w, g, b) =
  let wToWin = max 0 (3-w)
      gToWin = max 0 (4-g)
      bToWin = max 0 (5-b)
      cToWin = (max 0 (2-w)) + (max 0 (2-g)) + (max 0 (2-b)) in
    foldr (min) 100 [wToWin, gToWin, bToWin, cToWin]

-- occupiedCoords - Returns the list of coordinates that contain marbles
-- in a given game state.
occupiedCoords :: ZertzState -> [((Int, Int), HexState)]
occupiedCoords state@(ZertzState _ _ b _) =
    Map.toList $ Map.filter occupied b
  where
    occupied h = h /= Empty && h /= Open

-- successorJumps - Given a starting state, generate all states that are 
-- reachable through exactly one jump.
successorJumps :: ZertzState -> [ZertzState]
successorJumps ws@(ZertzState s1 s2 b p) = do
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

-- generateJumps - Given a starting state, generate the leaf states of the jump
-- tree inductively defined by successorJumps.
generateJumps :: ZertzState -> [ZertzState]
generateJumps state
   | null children = [state]
   | otherwise     = concatMap generateJumps children
   where children  = successorJumps state

-- generatePlacemetns - Given a starting state, generate all states that are
-- accessible through placing a marble and removing a disk.
-- FIXME: Implement disk-removal.
generatePlacements :: ZertzState -> [ZertzState]
generatePlacements s@(ZertzState s1 s2 b p) = do
    coord <- openHexes
    remCoord <- filter (removable s) openHexes
    color <- [White, Gray, Black]
    return $ ZertzState s1 s2 (placeMarble coord color b) p
  where
    openHexes = Map.keys $ Map.filter (== Open) b

removable :: ZertzState -> Coord -> Bool
removable (ZertzState _ _ b _) c =
  case numEmpties of
    1    -> True
    2    -> True
    3    -> True
    4    ->
      let offsetLists = map (\x -> drop x empties) [0..5]
          matches = 
            map (/= [True, True, True, False, True, False]) offsetLists in
        all (id) matches
    _    -> False
  where
    hexList = cycle [neHex, eHex, seHex, swHex, wHex, nwHex]
    empties = map (\x -> (getHex x b) == Empty) $ map ($ c) hexList
    numEmpties = length $ filter (id) $ take 6 empties
  