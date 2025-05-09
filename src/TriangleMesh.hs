{-# LANGUAGE TemplateHaskell #-}

module TriangleMesh (
    Vertex (..),
    Triangle (..),
    xCoord,
    yCoord,
    zCoord,
    vertex1,
    vertex2,
    vertex3,
    mapV,
    addV,
    dotProduct,
    vertexNormalize,
    vertexDiagonal,
    vertexLength,
    vertexFlip,
    isParallelTo,
    areColinear,
    xyCrossProduct,
    offsetNormal,
    vertexDistance,
    meshCeil,
    meshFloor
    )
    where

import Control.Lens
import Control.Parallel.Strategies
import Control.DeepSeq

data Vertex = Vertex {_xCoord, _yCoord, _zCoord :: Double} deriving Show
data Triangle = Triangle {_vertex1, _vertex2, _vertex3 :: Vertex} deriving (Show)

instance Eq Vertex where
    (Vertex x1 y1 z1) == (Vertex x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2

instance NFData Vertex where
    rnf (Vertex x y z) = rnf x `seq` rnf y `seq` rnf z

instance Eq Triangle where
    (Triangle v1 v2 v3) == (Triangle v1' v2' v3') = v1 == v1' && v2 == v2' && v3 == v3'

-- Manual Lenses to show that we are able to do that :)
xCoord :: Lens' Vertex Double
xCoord f (Vertex x y z) = (\x' -> Vertex x' y z) <$> (f x)

yCoord :: Lens' Vertex Double
yCoord f (Vertex x y z) = (\y' -> Vertex x y' z) <$> (f y)

zCoord :: Lens' Vertex Double
zCoord f (Vertex x y z) = (Vertex x y) <$> (f z)

vertex1 :: Lens' Triangle Vertex
vertex1 = lens _vertex1 (\triangle v1 -> triangle {_vertex1 = v1})

vertex2 :: Lens' Triangle Vertex
vertex2 = lens _vertex2 (\triangle v2 -> triangle {_vertex2 = v2})

vertex3 :: Lens' Triangle Vertex
vertex3 = lens _vertex3 (\triangle v3 -> triangle {_vertex3 = v3})



mapV :: (Double -> Double) -> Vertex -> Vertex
mapV f (Vertex x y z) = Vertex (f x) (f y) (f z)

addV :: Vertex -> Vertex -> Vertex
addV (Vertex x1 y1 z1) (Vertex x2 y2 z2) = Vertex (x1 + x2) (y1 + y2) (z1 + z2)

dotProduct :: Vertex -> Vertex -> Double
dotProduct (Vertex x1 y1 z1) (Vertex x2 y2 z2) = (x1*x2)+(y1*y2)+(z1*z2)

vertexLength :: Vertex -> Double
vertexLength (Vertex x y z) = sqrt(x**2 + y**2 + z**2)

-- Useful for our two dimensional slices in xy plane
xyCrossProduct :: Vertex -> Vertex -> Double
xyCrossProduct (Vertex x1 y1 _) (Vertex x2 y2 _) = x1*y2-y1*x2

vertexNormalize :: Vertex -> Vertex
vertexNormalize v@(Vertex x y z)
    | v == Vertex 0 0 0 = v
    | otherwise = Vertex (x/vlength) (y/vlength) (z/vlength)
        where vlength = vertexLength v

vertexFlip :: Vertex -> Vertex
vertexFlip = mapV (*(-1))

isParallelTo :: Vertex -> Vertex -> Bool
isParallelTo v1 v2 = xyCrossProduct v1 v2 == 0

-- Absolute Vertices (Points)
areColinear :: Vertex -> Vertex -> Vertex -> Bool
areColinear p1 p2 p3 = p1 `isParallelTo` p2 && p2 `isParallelTo` p3 && p1 `isParallelTo` p3

-- According to parallelogram addition Rules
-- Relative Vertices (Vectors)
vertexDiagonal :: Vertex -> Vertex -> Vertex
vertexDiagonal v1 v2 |v1 `isParallelTo` v2 = Vertex (v1^.yCoord) (mapV (*(-1)) v1^.xCoord) (v1^.zCoord)
                     |otherwise = vertexFlip v1 `addV` v2

-- Assuming anti-clockwise winding of Path
-- Relative Vertices (Vectors)
offsetNormal :: Vertex -> Vertex -> Vertex
offsetNormal v1 v2 = vertexNormalize $ flipToRight v1 $ vertexDiagonal (vertexNormalize v1) (vertexNormalize v2)

-- Assuming anti-clockwise winding of path
-- Relative Vertices (Vectors)
flipToRight :: Vertex -> Vertex -> Vertex
flipToRight v vn = if xyCrossProduct v vn > 0 then vertexFlip vn else vn

-- Calculates distance between vertices
vertexDistance :: Vertex -> Vertex -> Double
vertexDistance (Vertex x1 y1 z1) (Vertex x2 y2 z2) = sqrt $ (x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2

-- TODO: Eventually switch to -> Maybe Double
meshCeil :: [Triangle] -> Double
meshCeil ts = foldr (max . triangleCeil) (triangleCeil (head ts)) (tail ts)

triangleCeil :: Triangle -> Double
triangleCeil t = foldr max (view (vertex1.zCoord) t) [view (vertex2.zCoord) t, view (vertex3.zCoord) t]

-- TODO: Eventually switch to -> Maybe Double
meshFloor :: [Triangle] -> Double
meshFloor ts = foldr (min . triangleFloor) (triangleFloor (head ts)) (tail ts)

triangleFloor :: Triangle -> Double
triangleFloor t = foldr min (view (vertex1.zCoord) t) [view (vertex2.zCoord) t, view (vertex3.zCoord) t]