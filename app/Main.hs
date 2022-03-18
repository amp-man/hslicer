module Main where

import Lib3mf
import LibHslicer.PlanarSlice
import LibGcode
import System.Environment

main :: IO ()
main = do
    (a1:a2:as) <- getArgs
    let inputPath = a1 --read command line args
        outputPath = a2
    vertices <- parseVertices inputPath
    triangles <- parseTriangles inputPath vertices
    writeGCode outputPath (sliceMesh triangles sParamsDefault)
