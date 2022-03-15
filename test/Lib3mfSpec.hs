module Lib3mfSpec where

import Lib3mf
import TriangleMesh
import Test.Hspec (Spec, describe, context, it, shouldBe)
import Examples.RealCuboid

spec :: Spec
spec = do
    let path = "test/Lib3mfSpec_res/box_sliced/3D/3dmodel.model"

    it "parses vertices from 3mf file" $ do
        vertices <- parseVertices path
        vertices `shouldBe` [vertex_0,vertex_1,vertex_2,vertex_3,vertex_4,vertex_5,vertex_6,vertex_7]

    it "parses triangles from 3mf file" $ do
        vertices <- parseVertices path
        triangles <- parseTriangles path vertices
        triangles `shouldBe` cuboidMesh