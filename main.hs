module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import qualified Callbacks as Callbacks

-- NEW MAIN

initDisplay :: IO ()
initDisplay = do
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= Size 800 600
  createWindow "Zertz"
  
  Callbacks.registerCallbacks
  
  lineSmooth $= Enabled
  polygonSmooth $= Enabled
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  hint LineSmooth $= DontCare
  hint PolygonSmooth $= DontCare

main :: IO ()
main = do
  (progname, _) <- getArgsAndInitialize
  initDisplay
  mainLoop

-- OLD MAIN

-- run_minimax str_state =
--   let state :: Zertz.ZertzState
--      state = read str_state in
--  show $ MiniMax.minimax 100 state

-- prompt = do
--   putStr "Move? "
--   IO.hFlush IO.stdout
--   state <- getLine
--   putStrLn $ "\nState: " ++ (run_minimax state) ++ "\n"
--   prompt
  
-- oldmain = do
--   putStrLn $ "\nState: " ++ (show Zertz.startState) ++ "\n"
--   prompt