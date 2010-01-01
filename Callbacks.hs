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

unprojectCurrentMatrices :: Vertex3 GLdouble -> IO (Vertex3 GLdouble)
unprojectCurrentMatrices window_point= do
  projMatrix <- (get $ matrix $ Just Projection) :: IO (GLmatrix GLdouble)
  mvMatrix <- (get $ matrix $ Just $ Modelview 0) :: IO (GLmatrix GLdouble)
  vp <- get viewport
  unProject window_point mvMatrix projMatrix vp

mouseClick :: IORef Zertz.ZertzState -> (GLdouble, GLdouble) -> IO ()
mouseClick gameStateRef (clickX, clickY) = do
  print (clickX, clickY)
  postRedisplay Nothing

keyboardMouse :: IORef Zertz.ZertzState -> IORef (Maybe Position) -> KeyboardMouseCallback
keyboardMouse _ mouseStateRef (MouseButton LeftButton) Down
                (Modifiers  {shift= Up, ctrl = Up, alt = Up})
                position = do
  mouseStateRef $= Just position
keyboardMouse gameStateRef mouseStateRef (MouseButton LeftButton) Up
              (Modifiers  {shift= Up, ctrl = Up, alt = Up})
              cur_pos@(Position mouse_x mouse_y) = do
  mouseState <- readIORef mouseStateRef
  mouseStateRef $= Nothing
  case mouseState of
    Just old_pos | distance2 old_pos cur_pos < 25 -> do
      Vertex3 click_x click_y _ <- unprojectCurrentMatrices $
        Vertex3 (fromIntegral mouse_x) (fromIntegral mouse_y) 0.0
      mouseClick gameStateRef $ (click_x, click_y)
    _ ->
      return ()
  where
    distance2 :: Position -> Position -> GLint
    distance2 (Position x1 y1) (Position x2 y2) =
      (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)
keyboardMouse _ mouseStateRef (MouseButton _) _ _ _ = do
  mouseStateRef $= Nothing
-- keyboardMouse _ _ _ _ _ _ = return ()

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