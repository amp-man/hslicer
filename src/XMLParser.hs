{-# LANGUAGE OverloadedStrings #-}
module XMLParser where

import Prelude hiding (readFile)
import Text.XML
import Text.XML.Cursor
import qualified Data.Text as T

data Vertex = Vertex {x :: Double, y :: Double, z :: Double} deriving Show

data Triangle = Triangle {_v1 :: Vertex, _v2 :: Vertex, _v3 :: Vertex} deriving Show

verticesElement, vertexElement :: Name
verticesElement = Name "vertices" (Just "http://schemas.microsoft.com/3dmanufacturing/core/2015/02") Nothing
vertexElement = Name "vertex" (Just "http://schemas.microsoft.com/3dmanufacturing/core/2015/02") Nothing

parseVertices :: IO ()
parseVertices = do
    file <- readFile def "test\\box_sliced\\3D\\3dmodel.model"
    -- file <- readFile def "test\\box_sliced_0.2mm.gcode"
    let cursor = fromDocument file
    print $ zip3
            (cursor $// element verticesElement >=> child >=> element vertexElement >=> attribute "x")
            (cursor $// element verticesElement >=> child >=> element vertexElement >=> attribute "y")
            (cursor $// element verticesElement >=> child >=> element vertexElement >=> attribute "z")
                --Vertex {x = attribute "x", y = attribute "y", z = attribute "z"}
                --Vertex (attribute "x") (attribute "y") (attribute "z")
        -- >>= element vertexElement >>= descendant >>= content