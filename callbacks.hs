module Callbacks (registerCallbacks) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.IORef ( IORef, newIORef )

import qualified Geometry as Geometry
import qualified Zertz as Zertz

registerCallbacks :: IO ()
registerCallbacks = do
  gameState <- newIORef Zertz.startState
  keyboardMouseCallback $= Just (keyboardMouse gameState)
  displayCallback $= display gameState
  reshapeCallback $= Just reshape

keyboardMouse :: IORef Zertz.ZertzState -> KeyboardMouseCallback
keyboardMouse gameState key state modifiers position = do
  return ()

display :: IORef Zertz.ZertzState -> DisplayCallback
display gameState = do
  clear [ColorBuffer]
  lineWidth $= 3.0
  Geometry.renderFilledCircle
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