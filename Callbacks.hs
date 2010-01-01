module Callbacks (registerCallbacks) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.IORef ( IORef, newIORef, readIORef )

import qualified Render as Render
import qualified Zertz as Zertz
import qualified Data.Map as Map

registerCallbacks :: IO ()
registerCallbacks = do
  gameStateRef <- newIORef $ Zertz.ZertzState (0,0,0) (0,0,0) (Map.fromList [((-2,0),Zertz.Gray),((-2,1),Zertz.White),((-2,2),Zertz.Black),((-1,-1),Zertz.Open),((-1,0),Zertz.Open),((-1,1),Zertz.Open),((-1,2),Zertz.Open),((0,-2),Zertz.Open),((0,-1),Zertz.Open),((0,0),Zertz.Open),((0,1),Zertz.Open),((0,2),Zertz.Open),((1,-2),Zertz.Open),((1,-1),Zertz.Open),((1,0),Zertz.Open),((1,1),Zertz.Open),((2,-2),Zertz.Open),((2,-1),Zertz.Open),((2,0),Zertz.Open)]) (-1)
  mouseStateRef <- newIORef $ Nothing
  keyboardMouseCallback $= Just (keyboardMouse gameStateRef mouseStateRef)
  displayCallback $= display gameStateRef
  reshapeCallback $= Just reshape

keyboardMouse :: IORef Zertz.ZertzState -> IORef (Maybe Position) -> KeyboardMouseCallback
-- keyboardMouse gameStateRef mouseStateRef key state modifiers position
keyboardMouse _ mouseStateRef (MouseButton LeftButton) Down _ position = do
  mouseStateRef $= Just position
keyboardMouse _ mouseStateRef (MouseButton LeftButton) Up _ cur_pos = do
  mouseState <- readIORef mouseStateRef
  mouseStateRef $= Nothing
  case mouseState of
    Just old_pos | distance old_pos cur_pos < 25 ->
      print cur_pos
    _ ->
      return ()
  where
    distance :: Position -> Position -> GLint
    distance (Position x1 y1) (Position x2 y2) =
      (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)
    
keyboardMouse _ _ _ _ _ _ = do
  return ()

display :: IORef Zertz.ZertzState -> DisplayCallback
display gameStateRef = do
  Zertz.ZertzState firstScore secondScore board turn <- readIORef gameStateRef
  Render.renderZertzBoard board
  swapBuffers
  
reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho2D (-1) 1 (-1*hf/wf) (1*hf/wf)
      else ortho2D (-1*wf/hf) (1*wf/hf) (-1) 1
   matrixMode $= Modelview 0
   loadIdentity