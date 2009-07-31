module MiniMax(minimax, WorldState(generateMoves, isTerminal, score)) where

class WorldState a where
  generateMoves :: a -> [a]
  isTerminal :: a -> Bool
  score :: a -> Int

minimax :: WorldState a => Int -> a -> a
minimax max_depth world =
  snd $ negamax max_depth world (-100) 100 1
  where
     negamax depth node alpha beta color
        | depth == 0 = (color * (score node), node)
        | isTerminal node = (color * (score node), node)
        | otherwise = (alphabeta_prune alpha beta (generateMoves node), node)
      where
        alphabeta_prune alpha _ [] = alpha
        alphabeta_prune alpha beta (x:xs) =
          if ret >= beta
            then ret
            else alphabeta_prune ret beta xs
          where
            (subtree,_) = (negamax (depth-1) x (-beta) (-alpha) (-color))
            ret = max (-subtree) alpha
