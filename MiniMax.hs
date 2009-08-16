module MiniMax(minimax, WorldState(..)) where

class Show a => WorldState a where
  generateMoves :: a -> [a]
  isTerminal :: a -> Bool
  score :: a -> Int
  maxWS :: (Int, a) -> (Int, a) -> (Int, a)
  maxWS s1@(i1,_) s2@(i2,_)
    | i1 > i2   = s1
    | otherwise = s2

minimax :: WorldState a => Int -> a -> a
minimax max_depth world =
  snd $ foldr maxWS ((-1000), world) results
  where
    results = map (negamax max_depth 100 (-100) 1) (generateMoves world)
    negamax depth alpha beta color node
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
             (subtree,_) = negamax (depth-1) (-beta) (-alpha) (-color) x
             ret = max (-subtree) alpha
