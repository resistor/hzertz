module Main where

import qualified IO as IO
import qualified Zertz as Zertz
import qualified ZertzAI as ZertzAI
import qualified MiniMax as MiniMax

run_minimax str_state =
  let state :: Zertz.ZertzState
      state = read str_state in
  show $ MiniMax.minimax 100 state

prompt = do
  putStr "Move? "
  IO.hFlush IO.stdout
  state <- getLine
  putStrLn $ "\nState: " ++ (run_minimax state) ++ "\n"
  prompt
  
main = do
  putStrLn $ "\nState: " ++ (show Zertz.startState) ++ "\n"
  prompt