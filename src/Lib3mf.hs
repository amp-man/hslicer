module Lib3mf
    --  ( Vertex(..),
    --   Triangle(..),
    --   someFunc
    --) 
    where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Gcode = ABSMOVE Coordinates 
           | RELMOVE Coordinates 
           | CUSTOM String

data Coordinates = Coord {_x :: Double, _y :: Double, _z :: Double, _e :: Double, _f :: Double} deriving Show
