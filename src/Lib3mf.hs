{-# LANGUAGE OverloadedStrings #-}

module Lib3mf (
    parseVertices,
    parseTriangles
    )
    where

import Prelude hiding (readFile)
import qualified Data.Text as T
import Text.XML ( Name(Name), readFile, def )
import Text.XML.Cursor ( (>=>), attribute, element, fromDocument, ($//), (&//) )
import TriangleMesh

-- Read all vertices from 3mf model xml
parseVertices :: FilePath -> IO [Vertex]
parseVertices path = do
    file <- readFile def path
    let cursor = fromDocument file
    return $
        cursor $// element vertices &// element vertex >=> 
            \axis -> [Vertex {_xCoord = convertToDouble $ attribute "x" axis, _yCoord = convertToDouble $ attribute "y" axis, _zCoord = convertToDouble $ attribute "z" axis}]

-- Read all triangles from 3mf model xml
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

-- According to 3mf spec
nameSpace3mf :: T.Text
nameSpace3mf = "http://schemas.microsoft.com/3dmanufacturing/core/2015/02"

vertices, vertex, triangles, triangle :: Name
vertices = Name "vertices" (Just nameSpace3mf) Nothing
vertex = Name "vertex" (Just nameSpace3mf) Nothing
triangles = Name "triangles" (Just nameSpace3mf) Nothing
triangle = Name "triangle" (Just nameSpace3mf) Nothing