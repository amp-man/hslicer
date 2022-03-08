module TriangleMeshSpec where

import TriangleMesh
import Test.Hspec (Spec, describe, context, it, shouldBe)

spec :: Spec
spec = do
    let v1 = Vertex 2.0 1.0 0.0
        v2 = Vertex 1.0 2.0 0.0
        v1v2odiag = Vertex 1.0 (-1.0) 0.0
        vra1 = Vertex 10 0 0
        vra2 = Vertex 0 10 0
        vradiag = Vertex 1.0 (-1.0) 0.0
    it "flips vertex" $
        vertexFlip (Vertex (-1.0) (-2.0) 0.0) `shouldBe` Vertex 1.0 2.0 0.0
    it "calculates length of vertex" $
        vertexLength (Vertex 1.0 2.0 0.0) `shouldBe` sqrt 5
    it "normalizes vertex" $
        vertexNormalize (Vertex 3.0 1.0 2.0) `shouldBe` Vertex (3.0/sqrt 14) (1.0/sqrt 14) (2.0/sqrt 14)
    it "calculates parallelogram diagonal of two convex vertices (vectors turn left)" $
        vertexDiagonal v1 v2 `shouldBe` Vertex (-1.0) 1.0 0.0
    it "calculates parallelogram diagonal of two concave vertices (vectors turn right)" $
        vertexDiagonal v2 v1 `shouldBe` Vertex 1.0 (-1.0) 0.0
    it "calculates offset normal of two convex vertices (vectors turn left)" $
        offsetNormal v1 v2 `shouldBe` vertexNormalize v1v2odiag
    it "calculates offset normal of two convex right angle vertices" $
        offsetNormal vra1 vra2 `shouldBe` vertexNormalize vradiag
    it "calculates offset normal of two concave vertices (vectors turn right)" $
        offsetNormal v2 v1 `shouldBe` vertexNormalize v1v2odiag