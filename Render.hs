module Render (renderZertzBoard) where

import Graphics.Rendering.OpenGL

import qualified Data.Map as Map
import qualified Zertz as Zertz

renderZertzBoard :: Zertz.ZertzBoard -> IO ()
renderZertzBoard board = do
  clear [ColorBuffer]
  lineWidth $= 3.0
  mapM_ (preservingMatrix . renderZertzHex) $ Map.toList board
  
renderZertzHex :: (Zertz.Coord, Zertz.HexState) -> IO ()
renderZertzHex (_, Zertz.Empty) = return ()
renderZertzHex ((x, y), state) = do
  color $ Color3 (1.0::GLfloat) (1.0::GLfloat) (1.0::GLfloat) 
  scale scaleFrac scaleFrac scaleFrac
  translate $ (Vector3 (renderX + (renderY / 2.0)) ((sqrt 3.0) * renderY / 2.0) 0.0)
  renderHollowHexagon
  
  scale (0.75::GLfloat) (0.75::GLfloat) (0.75::GLfloat)
  case state of
    Zertz.White ->
      color $ Color3 (1.0::GLfloat) (1.0::GLfloat) (1.0::GLfloat) 
    Zertz.Gray ->
      color $ Color3 (0.6::GLfloat) (0.6::GLfloat) (0.6::GLfloat) 
    Zertz.Black ->
      color $ Color3 (0.2::GLfloat) (0.2::GLfloat) (0.2::GLfloat)
    Zertz.Open ->
      color $ Color3 (0.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat)
  renderFilledCircle
  
  where
    scaleFrac = (1.0 / 6.0) :: GLfloat
    renderX = 2.0 * fromIntegral x :: GLfloat
    renderY = 2.0 * fromIntegral y :: GLfloat

regularNGon :: Int -> [Vertex2 GLfloat]
regularNGon vertices =
  map (\(x, y) -> Vertex2 x y) unitCircle
  where
    dAlpha = 2.0 * pi / (fromIntegral vertices)
    angles = map (+ (dAlpha / 2.0)) $ map ((* dAlpha) . fromIntegral) [0 .. vertices-1]
    unitCircle = map (\a -> (cos a, sin a)) angles

verticesToEdges :: [Vertex2 GLfloat] -> [Vertex2 GLfloat]
verticesToEdges vertices = 
  take (2 * (length vertices)) flattenedList
  where
    circularList = cycle vertices
    zippedList = zip circularList $ tail circularList
    flattenedList = concatMap (\(x, y) -> [x, y]) zippedList

renderHollowHexagon :: IO()
renderHollowHexagon = do
  renderPrimitive Lines $
    mapM_ vertex $ verticesToEdges $ regularNGon 6

renderFilledCircle :: IO()
renderFilledCircle = do
  renderPrimitive TriangleFan $
    mapM_ vertex $ [Vertex2 0.0 0.0] ++ ngon ++ [head ngon]
  renderPrimitive Lines $
    mapM_ vertex $ verticesToEdges $ ngon
  where
    ngon = regularNGon 180