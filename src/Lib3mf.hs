{-# LANGUAGE OverloadedStrings #-}
module Lib3mf (
    Vertex (..),
    Triangle (..),
    parseVertices,
    parseTriangles
    )
    where

import Prelude hiding (readFile)
import Text.XML
import Text.XML.Cursor
import qualified Data.Text as T

data Vertex = Vertex {_x :: Double, _y :: Double, _z :: Double} deriving Show

instance Eq Vertex where
  (Vertex x1 y1 z1) == (Vertex x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2

data Triangle = Triangle {_v1 :: Vertex, _v2 :: Vertex, _v3 :: Vertex} deriving Show

instance Eq Triangle where
    (Triangle v1 v2 v3) == (Triangle v1' v2' v3') = v1 == v1' && v2 == v2' && v3 == v3'

parseVertices :: FilePath -> IO [Vertex]
parseVertices path = do
    file <- readFile def path
    let cursor = fromDocument file
    return $
        cursor $// element vertices &// element vertex >=> 
            \axis -> [Vertex {_x = convertToDouble $ attribute "x" axis, _y = convertToDouble $ attribute "y" axis, _z = convertToDouble $ attribute "z" axis}]

parseTriangles :: FilePath -> [Vertex] -> IO [Triangle]
parseTriangles path vs = do
    file <- readFile def path
    let cursor = fromDocument file
    return $
        cursor $// element triangles &// element triangle >=>
            \axis -> [Triangle {_v1 = deriveVertex (attribute "v1" axis) vs, _v2 = deriveVertex (attribute "v2" axis) vs, _v3 = deriveVertex (attribute "v3" axis) vs}]


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