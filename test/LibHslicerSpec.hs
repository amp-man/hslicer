module LibHslicerSpec where

import LibHslicer
import Lib3mf
import Test.Hspec (Spec, describe, context, it, shouldBe)

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