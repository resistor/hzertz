module Geometry (renderHollowHexagon, renderFilledCircle) where

import Graphics.Rendering.OpenGL

regularNGon :: Int -> [Vertex2 GLfloat]
regularNGon vertices =
  map (\(x, y) -> Vertex2 x y) unitCircle
  where
    dAlpha = 2.0 * pi / (fromIntegral vertices)
    angles = map ((* dAlpha) . fromIntegral) [0 .. vertices-1]
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