module IntegrationSpec where

import Lib3mf
import TriangleMesh
import LibHslicer.Contour
import Test.Hspec (Spec, describe, context, it, shouldBe)

spec :: Spec
spec = do
    let path = "test/Lib3mfSpec_res/sphere/3D/3dmodel.model"
    it "parses triangles from 3mf file and generates contour" $ do
        vertices <- parseVertices path
        triangles <- parseTriangles path vertices
        print (show (generateContour triangles 0.0))