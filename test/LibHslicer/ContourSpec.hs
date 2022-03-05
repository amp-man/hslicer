module LibHslicer.ContourSpec where

import LibHslicer.Contour
import Lib3mf
import Test.Hspec (Spec, describe, context, it, shouldBe)
import Control.Lens

spec :: Spec
spec = do
    let v1 = Vertex 0.0 0.0 2.0
        v2 = Vertex 0.0 1.0 0.0
        v3 = Vertex 0.0 0.0 0.0
        v4 = Vertex 0.0 1.0 2.0
        v5 = Vertex 1.0 1.0 0.0
        v6 = Vertex 1.0 1.0 2.0
        v7 = Vertex 1.0 0.0 0.0
        v8 = Vertex 1.0 0.0 2.0
        t1 = Triangle v1 v2 v3
        t2 = Triangle v1 v2 v4
        t3 = Triangle v1 v3 v7
        t4 = Triangle v1 v7 v8
        t5 = Triangle v2 v4 v5
        t6 = Triangle v4 v5 v6
        t7 = Triangle v5 v6 v7
        t8 = Triangle v6 v7 v8
        t9 = Triangle v2 v3 v5
        t10 = Triangle v3 v5 v7
        t11 = Triangle v1 v4 v6
        t12 = Triangle v1 v6 v8
        mesh = [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12]
        intTri1 = IntersecTriangle t1 [Vertex 0.0 0.0 1.0, Vertex 0.0 0.5 1.0] -- zSlice = 1.0
        intTri2 = IntersecTriangle t2 [Vertex 0.0 0.5 1.0, Vertex 0.0 1.0 1.0] -- zSlice = 1.0
        intTri3 = IntersecTriangle t3 [Vertex 0.0 0.0 1.0, Vertex 0.5 0.0 1.0] -- zSlice = 1.0
        intTri4 = IntersecTriangle t4 [Vertex 0.5 0.0 1.0, Vertex 1.0 0.0 1.0] -- zSlice = 1.0
        intTri5 = IntersecTriangle t5 [Vertex 0.0 1.0 1.0, Vertex 0.5 1.0 1.0] -- zSlice = 1.0
        intTri6 = IntersecTriangle t6 [Vertex 0.5 1.0 1.0, Vertex 1.0 1.0 1.0] -- zSlice = 1.0
        intTri7 = IntersecTriangle t7 [Vertex 1.0 1.0 1.0, Vertex 1.0 0.5 1.0] -- zSlice = 1.0
        intTri8 = IntersecTriangle t8 [Vertex 1.0 0.5 1.0, Vertex 1.0 0.0 1.0] -- zSlice = 1.0
        intTris = [intTri1, intTri2, intTri3, intTri4, intTri5, intTri6, intTri7, intTri8]
        path = [intTri1, intTri2, intTri5, intTri6, intTri7, intTri8, intTri4, intTri3, intTri1]
    it "detects intersection z1 > z* > z2" $
       isIntersectingVertex v2 v1 0.5 `shouldBe` True
    it "detects intersection z2 > z* > z1" $
       isIntersectingVertex v2 v1 0.5 `shouldBe` True
    it "detects intersection z1 = z* = z2" $
       isIntersectingVertex v1 v4 2.0 `shouldBe` True
    it "detects no intersection" $
       isIntersectingVertex v1 v2 3.0 `shouldBe` False
    it "calculates vertex intersection z1 > z* > z2" $
       calcIntersecVertex v1 v2 0.5 `shouldBe` Just (Right (Vertex 0.0 0.75 0.5))
    it "calculates vertex intersection z2 > z* > z1" $
       calcIntersecVertex v2 v1 0.5 `shouldBe` Just (Right (Vertex 0.0 0.75 0.5))
    it "calculates vertex intersection z1 = z* = z2" $
       calcIntersecVertex v1 v4 2.0 `shouldBe` Just (Left (v1, v4))
    it "calculates no vertex intersection" $
       calcIntersecVertex v1 v2 3.0 `shouldBe` Nothing
    it "detects triangle intersection 1 point" $
       isIntersectingTriangle t1 2.0 `shouldBe` True
    it "detects triangle intersection 2 points" $
       isIntersectingTriangle t1 0.5 `shouldBe` True
    it "detects triangle intersection 3 points" $
       isIntersectingTriangle t11 2.0 `shouldBe` True
    it "detects no triangle intersection" $
       isIntersectingTriangle t1 3.0 `shouldBe` False
    it "calulates triangle intersection 1 point" $
       calcIntersecTriangle t1 2.0 `shouldBe` IntersecTriangle t1 [v1]
    it "calculates triangle intersection 2 points" $
       calcIntersecTriangle t1 1.0 `shouldBe` intTri1
    it "calculates triangle intersection 3 points" $
       calcIntersecTriangle t11 2.0 `shouldBe` IntersecTriangle t11 [v1, v4, v6]
    it "calculates all triangle intersections" $
       map (`calcIntersecTriangle` 1.0) [t1,t2,t3,t4,t5,t6,t7,t8] `shouldBe` intTris
    it "calculates all triangle 2 point intersections" $
       filter (\intTri -> length (intTri & view intersections) == 2) (map (`calcIntersecTriangle` 1.0) mesh) `shouldBe` intTris
    it "finds connection no done" $
       findConnection intTri1 (remove intTri1 intTris) [] `shouldBe` intTri2
    it "finds connection midway" $
       findConnection intTri2 [intTri3, intTri4, intTri5, intTri6, intTri7, intTri8] [intTri1] `shouldBe` intTri5
    it "finds connection closing circle (no todo)" $
       findConnection (IntersecTriangle t3 [Vertex 0.5 0.0 1.0, Vertex 0.0 0.0 1.0]) [] [intTri1, intTri2, intTri5, intTri6, intTri7, intTri8, intTri4] `shouldBe` intTri1
    it "generates path" $
       createCoherentPath intTris `shouldBe` reverse path
    --it "generates contour" $ do
       --print (generateContour mesh 0.1)