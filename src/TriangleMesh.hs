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
    dot,
    vertexNormalize,
    vertexDiagonal,
    vertexLength,
    vertexFlip,
    offsetNormal
    )
    where

import Control.Lens (makeLenses)

data Vertex = Vertex {_xCoord, _yCoord, _zCoord :: Double} deriving Show
data Triangle = Triangle {_vertex1, _vertex2, _vertex3 :: Vertex} deriving (Show)

instance Eq Vertex where
    (Vertex x1 y1 z1) == (Vertex x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2

instance Eq Triangle where
    (Triangle v1 v2 v3) == (Triangle v1' v2' v3') = v1 == v1' && v2 == v2' && v3 == v3'

makeLenses ''Vertex
makeLenses ''Triangle

mapV :: (Double -> Double) -> Vertex -> Vertex 
mapV f (Vertex x y z) = Vertex (f x) (f y) (f z)

addV :: Vertex -> Vertex -> Vertex
addV (Vertex x1 y1 z1) (Vertex x2 y2 z2) = Vertex (x1 + x2) (y1 + y2) (z1 + z2)

dot :: Vertex -> Vertex -> Double
dot (Vertex x1 y1 z1) (Vertex x2 y2 z2) = (x1*x2)+(y1*y2)+(z1*z2)

vertexLength :: Vertex -> Double
vertexLength (Vertex x y z) = sqrt(x**2 + y**2 + z**2)

-- TODO: Should be two dimensional Vertex only
xyCrossProduct :: Vertex -> Vertex -> Double
xyCrossProduct (Vertex x1 y1 _) (Vertex x2 y2 _) = x1*y2-y1*x2

vertexNormalize :: Vertex -> Vertex
vertexNormalize v@(Vertex x y z) = Vertex (x/vlength) (y/vlength) (z/vlength)
    where vlength = vertexLength v

vertexFlip :: Vertex -> Vertex
vertexFlip = mapV (*(-1))

-- According to parallelogram addition Rules
-- Relative Vertices
vertexDiagonal :: Vertex -> Vertex -> Vertex
vertexDiagonal v1 v2 = vertexFlip v1 `addV` v2

-- Assuming anti-clockwise winding of Path
-- Relative Vertices
offsetNormal :: Vertex -> Vertex -> Vertex
offsetNormal v1 v2 = vertexNormalize $ flipToRight v1 $ vertexDiagonal v1 v2

-- Assuming anti-clockwise winding of path
-- Relative Vertices
flipToRight :: Vertex -> Vertex -> Vertex
flipToRight v vn = if xyCrossProduct v vn > 0 then vertexFlip vn else vn
