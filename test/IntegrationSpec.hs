module IntegrationSpec where

import Lib3mf
import TriangleMesh
import LibHslicer.Contour
import Test.Hspec (Spec, describe, context, it, shouldBe)
import LibHslicer.PlanarSlice
import LibHslicer.PlanarSlice (sliceContours)


spec :: Spec
spec = do
    let path = "test/Lib3mfSpec_res/sphere/3D/3dmodel.model"
        path2 = "test/Lib3mfSpec_res/Polygon/3D/3dmodel.model"
    it "Unneccessary test" $ do
        print "DONUT"
    -- it "parses triangles from 3mf file and generates contour" $ do
    --     vertices <- parseVertices path
    --     triangles <- parseTriangles path vertices
    --     print (show (generateContour triangles 0.0))
    -- it "should not be possible to have contours with smaller than 3 vertices" $ do
    --     vertices <- parseVertices path2
    --     triangles <- parseTriangles path2 vertices
    --     (any ((< 4) . (length . take 4)) (sliceContours triangles sParamsDefault)) `shouldBe` False
    --     --print (show (generateContour triangles 50.0))