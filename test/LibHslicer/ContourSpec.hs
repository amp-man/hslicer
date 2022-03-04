module LibHslicer.ContourSpec where

import LibHslicer.Contour
import Lib3mf
import Test.Hspec (Spec, describe, context, it, shouldBe)
import LibHslicer.Contour (calculateOffsetForContour)

spec :: Spec
spec = do
    let v1 = Vertex 0.0 0.0 2.0
        v2 = Vertex 0.0 1.0 0.0
        v3 = Vertex 0.0 0.0 0.0
        v4 = Vertex 0.0 1.0 2.0
        t1 = Triangle v1 v2 v3
        t2 = Triangle v1 v2 v4
        t3 = Triangle v2 v3 v4
        intTri1 = IntersecTriangle t1 [v3, v1]
        intTri2 = IntersecTriangle t2 [v2, v1]
        intTri3 = IntersecTriangle t3 [v2, v3]
        -- TODO: Should be points with 2 Dimensions only
        rectangle = [Vertex 0 0 0, Vertex 10 0 0, Vertex 10 10 0, Vertex 0 10 0]
        -- TODO: Should be points with 2 Dimensions only
        smallrectangle = [Vertex 1 1 0, Vertex 9 1 0, Vertex 9 9 0, Vertex 1 9 0]
    it "detects intersection z1 > z* > z2" $
       isIntersectingVertex v2 v1 0.5 `shouldBe` True
    it "detects intersection z2 > z* > z1" $
       isIntersectingVertex v2 v1 0.5 `shouldBe` True
    it "detects no intersection" $
       isIntersectingVertex v1 v2 3.0 `shouldBe` False
    it "calculates vertex intersection z1 > z* > z2" $
       calcIntersecVertex v1 v2 0.5 `shouldBe` Just (Vertex 0.0 0.75 0.5)
    it "calculates vertex intersection z2 > z* > z1" $
      calcIntersecVertex v2 v1 0.5 `shouldBe` Just (Vertex 0.0 0.75 0.5)
    it "calculates no vertex intersection" $
       calcIntersecVertex v1 v2 3.0 `shouldBe` Nothing
    it "finds connection" $
       findConnection intTri1 [intTri2, intTri3] ([] :: [IntersecTriangle]) `shouldBe` IntersecTriangle t2 [v1, v2]
    it "offsets a path section Midpoint by -1" $
       calculateOffsetForPoint (-1) (Vertex 0 0 0) (Vertex 10 0 0) (Vertex 10 10 0) `shouldBe` Vertex 9 1 0
    it "offsets a path section Midpoint by -2" $
       calculateOffsetForPoint (-2) (Vertex 0 0 0) (Vertex 10 0 0) (Vertex 10 10 0) `shouldBe` Vertex 8 2 0
    it "offsets a path section Midpoint by -2" $
       calculateOffsetForPoint (-2) (Vertex 10 0 0) (Vertex 10 10 0) (Vertex 0 10 0) `shouldBe` Vertex 8 8 0
    it "offsets a path section Midpoint by -2" $
       calculateOffsetForPoint (-2) (Vertex 10 10 0) (Vertex 0 10 0) (Vertex 0 0 0) `shouldBe` Vertex 2 8 0
    it "offsets a path section Midpoint by -1" $
       calculateOffsetForPoint (-1) (Vertex 10 10 0) (Vertex 0 10 0) (Vertex 0 0 0) `shouldBe` Vertex 1 9 0
    it "offsets a contour by -1" $
       calculateOffsetForContour (-1) rectangle `shouldBe` smallrectangle
       