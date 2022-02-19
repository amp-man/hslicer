module Lib
    --  ( Vertex(..),
    --   Triangle(..),
    --   someFunc
    --) 
    where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Vertex = Vertex {x :: Double, y :: Double, z :: Double} deriving Show

data Triangle = Triangle {_v1 :: Vertex, _v2 :: Vertex, _v3 :: Vertex} deriving Show

data Gcode = ABSMOVE Coordinates 
           | RELMOVE Coordinates 
           | CUSTOM String

data Coordinates = Coord {_x :: Double, _y :: Double, _z :: Double, _e :: Double, _f :: Double} deriving Show