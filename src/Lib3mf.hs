{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib3mf (
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
    vertexNormalize,
    vertexDiagonal,
    vertexLength,
    vertexFlip,
    offsetNormal,
    parseVertices,
    parseTriangles
    )
    where

import Prelude hiding (readFile)
import qualified Data.Text as T
import Text.XML ( Name(Name), readFile, def )
import Text.XML.Cursor ( (>=>), attribute, element, fromDocument, ($//), (&//) )
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

vertexLength :: Vertex -> Double
vertexLength (Vertex x y z) = sqrt(x**2 + y**2 + z**2)

xyCrossProduct :: Vertex -> Vertex -> Double
xyCrossProduct (Vertex x1 y1 z1) (Vertex x2 y2 z2) = x1*y2-y1*x2

vertexNormalize :: Vertex -> Vertex
vertexNormalize v@(Vertex x y z) = Vertex (x/vlength) (y/vlength) (z/vlength)
    where vlength = vertexLength v

vertexFlip :: Vertex -> Vertex
vertexFlip = mapV (*(-1))

-- According to parallelogram addition Rules
vertexDiagonal :: Vertex -> Vertex -> Vertex
vertexDiagonal v1 v2 = vertexFlip v1 `addV` v2

-- Assuming anti-clockwise winding of Path
offsetNormal :: Vertex -> Vertex -> Vertex
offsetNormal v1 v2 = vertexNormalize $ leftifyNormal v1 $ vertexDiagonal v1 v2

-- Assuming anti-clockwise winding of path
leftifyNormal :: Vertex -> Vertex -> Vertex
leftifyNormal v vn = if xyCrossProduct v vn > 0 then vertexFlip vn else vn

parseVertices :: FilePath -> IO [Vertex]
parseVertices path = do
    file <- readFile def path
    let cursor = fromDocument file
    return $
        cursor $// element vertices &// element vertex >=> 
            \axis -> [Vertex {_xCoord = convertToDouble $ attribute "x" axis, _yCoord = convertToDouble $ attribute "y" axis, _zCoord = convertToDouble $ attribute "z" axis}]

parseTriangles :: FilePath -> [Vertex] -> IO [Triangle]
parseTriangles path vs = do
    file <- readFile def path
    let cursor = fromDocument file
    return $
        cursor $// element triangles &// element triangle >=>
            \axis -> [Triangle {_vertex1 = deriveVertex (attribute "v1" axis) vs, _vertex2 = deriveVertex (attribute "v2" axis) vs, _vertex3 = deriveVertex (attribute "v3" axis) vs}]


convertToDouble :: [T.Text] -> Double
convertToDouble text = read $ T.unpack $ T.concat text ::Double

deriveVertex :: [T.Text] -> [Vertex] -> Vertex
deriveVertex index list = list !! (read $ T.unpack $ T.concat index :: Int)

nameSpace3mf :: T.Text
nameSpace3mf = "http://schemas.microsoft.com/3dmanufacturing/core/2015/02"

vertices, vertex, triangles, triangle :: Name
vertices = Name "vertices" (Just nameSpace3mf) Nothing
vertex = Name "vertex" (Just nameSpace3mf) Nothing
triangles = Name "triangles" (Just nameSpace3mf) Nothing
triangle = Name "triangle" (Just nameSpace3mf) Nothing