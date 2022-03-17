module TriangleMeshSpec where

import TriangleMesh
import Test.Hspec (Spec, describe, context, it, shouldBe, shouldThrow, anyErrorCall)
import Test.QuickCheck
import Control.Exception (evaluate)

spec :: Spec
spec = do
    let v1 = Vertex 2.0 1.0 0.0
        v2 = Vertex 1.0 2.0 0.0
        v1v2odiag = Vertex 1.0 (-1.0) 0.0
        vra1 = Vertex 10 0 0
        vra2 = Vertex 0 10 0
        vradiag = Vertex 1.0 (-1.0) 0.0

        t1 = Triangle (Vertex 0.0 0.0 0.0) (Vertex 0.0 1.0 0.0) (Vertex 1.0 0.0 0.0)
        t2 = Triangle (Vertex 0.0 0.0 0.0) (Vertex 0.0 1.0 0.0) (Vertex 0.0 0.0 1.0)
    
    describe "TriangleMesh.mapV" $ do
        it "maps (+i) to all vertex coordinates" $ 
          property $
            \x y z i -> mapV (+ i) (Vertex x y z) `shouldBe` Vertex (i + x) (i + y) (i + z)
        it "maps (*i) to all vertex coordinates" $
          property $
            \x y z i -> mapV (* i) (Vertex x y z) `shouldBe` Vertex (i * x) (i * y) (i * z)
    
    describe "TriangleMesh.addV" $ do
        it "adds 2 arbitrary vertices" $
          property $
            \x1 y1 z1 x2 y2 z2  -> addV (Vertex x1 y1 z1) (Vertex x2 y2 z2) `shouldBe` Vertex (x1 + x2) (y1 + y2) (z1 + z2)
        it "neutralizes 2 opposing vertices" $
          property $
            \x y z -> addV (Vertex x y z) (Vertex (-x) (-y) (-z)) `shouldBe` Vertex 0.0 0.0 0.0

    describe "TriangleMesh.vertexFlip" $ do
        it "flips vertex concrete example" $
            vertexFlip (Vertex (-1.0) (-2.0) 0.0) `shouldBe` Vertex 1.0 2.0 0.0
        it "flips arbitrary vertex" $
          property $
            \x y z -> vertexFlip (Vertex x y z) `shouldBe` Vertex (- x) (- y) (- z)

    describe "TriangleMesh.vertexLength" $ do
        it "calculates length of vertex concrete example" $
            vertexLength (Vertex 1.0 2.0 0.0) `shouldBe` sqrt 5
        it "calculates length of arbitrary vertex" $
          property $
            \x y z -> vertexLength (Vertex x y z) `shouldBe` sqrt (x**2 + y**2 + z**2)
    
    describe "TriangleMesh.vertexNormalize" $ do
        it "normalizes vertex concrete example" $
            vertexNormalize (Vertex 3.0 1.0 2.0) `shouldBe` Vertex (3.0/sqrt 14) (1.0/sqrt 14) (2.0/sqrt 14)
        it "normalizes arbitrary vertex" $ -- test fails
          property $
            \x y z -> let v = Vertex x y z
                          l = vertexLength v
                      in vertexNormalize v `shouldBe` if v == Vertex 0 0 0
                                                        then v
                                                        else Vertex (x/l) (y/l) (z/l)
    
    describe "TriangleMesh.vertexDiagonal" $ do
        it "calculates parallelogram diagonal of two convex vertices (vectors turn left)" $
            vertexDiagonal v1 v2 `shouldBe` Vertex (-1.0) 1.0 0.0
        it "calculates parallelogram diagonal of two concave vertices (vectors turn right)" $
            vertexDiagonal v2 v1 `shouldBe` Vertex 1.0 (-1.0) 0.0
    
    describe "TriangleMesh.offsetNormal" $ do
        it "calculates offset normal of two convex vertices (vectors turn left)" $
            offsetNormal v1 v2 `shouldBe` vertexNormalize v1v2odiag
        it "calculates offset normal of two convex right angle vertices" $
            offsetNormal vra1 vra2 `shouldBe` vertexNormalize vradiag
        it "calculates offset normal of two concave vertices (vectors turn right)" $
            offsetNormal v2 v1 `shouldBe` vertexNormalize v1v2odiag
    
    describe "TriangleMesh.vertexDistance" $ do
        it "calculates distance between vertices" $
            vertexDistance v1 v2 `shouldBe` sqrt 2
    
    describe "TriangleMesh.meshCeil" $ do
        it "calculates Ceiling of triangle mesh" $
            meshCeil [t1, t2] `shouldBe` 1
    
    describe "TriangleMesh.meshFloor" $ do
        it "calculates floor of triangle mesh" $
            meshFloor [t1, t2] `shouldBe` 0
        it "Throws Error on no mesh" $
            evaluate (meshFloor []) `shouldThrow` anyErrorCall