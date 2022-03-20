module LibHslicer.ContourSpec where

import LibHslicer.Contour
import TriangleMesh
import Test.Hspec (Spec, describe, context, it, shouldBe)
import Control.Lens
import Examples.ReferenceMesh

spec :: Spec
spec = do
   let
      rectangle = [Vertex 0 0 0, Vertex 10 0 0, Vertex 10 10 0, Vertex 0 10 0, Vertex 0 0 0]
      rectangle_offset1 = [Vertex 1 1 0, Vertex 9 1 0, Vertex 9 9 0, Vertex 1 9 0, Vertex 1 1 0]
      testpath = [Vertex 0.100005 20.007000 0.000000, Vertex 0.200000 0.399980 0.000000, Vertex 10.003995 0.200000 0.000000, Vertex 9.904000 19.807020 0.000000, Vertex 0.100005 20.007000 0.000000]
      testpath1 = [Vertex 0.000000 0.133344 0.200000, Vertex 0.000000 0.000000 0.200000, Vertex 10.037325 0.000000 0.200000, Vertex 10.104000 0.000000 0.200000, Vertex 10.104000 20.073656 0.200000, Vertex 10.104000 20.207000 0.200000, Vertex 0.066675 20.207000 0.200000, Vertex 0.000000 20.207000 0.200000, Vertex 0.000000 0.133344 0.200000]
      testpathcw = [Vertex 0.000000 0.000000 30.308000, Vertex 0.000000 20.207000 30.308000, Vertex 10.104000 20.207000 30.308000, Vertex 10.104000 0.000000 30.308000, Vertex 0.000000 0.000000 30.308000]
      rotatedrectangle = [Vertex 10 10 0, Vertex 0 10 0, Vertex 0 0 0, Vertex 10 0 0, Vertex 10 10 0]

   describe "LibHslicer.Contour.isIntersectingVertex" $ do
      it "detects intersection z1 > z* > z2" $
         isIntersectingVertex v2 v1 0.5 `shouldBe` True
      it "detects intersection z2 > z* > z1" $
         isIntersectingVertex v2 v1 0.5 `shouldBe` True
      it "detects intersection z1 = z* = z2" $
         isIntersectingVertex v1 v4 2.0 `shouldBe` True
      it "detects no intersection" $
         isIntersectingVertex v1 v2 3.0 `shouldBe` False

   describe "LibHslicer.Contour.calcIntersecVertex" $ do
      it "calculates vertex intersection z1 > z* > z2" $
         calcIntersecVertex v1 v2 0.5 `shouldBe` Just (Right (Vertex 0.0 0.75 0.5))
      it "calculates vertex intersection z2 > z* > z1" $
         calcIntersecVertex v2 v1 0.5 `shouldBe` Just (Right (Vertex 0.0 0.75 0.5))
      it "calculates vertex intersection z1 = z* = z2" $
         calcIntersecVertex v1 v4 2.0 `shouldBe` Just (Left (v1, v4))
      it "calculates no vertex intersection" $
         calcIntersecVertex v1 v2 3.0 `shouldBe` Nothing

   describe "LibHslicer.Contour.isIntersectingTriangle" $ do
      it "detects triangle intersection 1 point" $
         isIntersectingTriangle t1 2.0 `shouldBe` True
      it "detects triangle intersection 2 points" $
         isIntersectingTriangle t1 0.5 `shouldBe` True
      it "detects triangle intersection 3 points" $
         isIntersectingTriangle t11 2.0 `shouldBe` True
      it "detects no triangle intersection" $
         isIntersectingTriangle t1 3.0 `shouldBe` False

   describe "LibHslicer.Contour.calcIntersecTriangle" $ do
      it "calulates triangle intersection 1 point" $
         calcIntersecTriangle t1 2.0 `shouldBe` IntersecTriangle t1 [v1]
      it "calculates triangle intersection 2 points" $
         calcIntersecTriangle t1 1.0 `shouldBe` intTri1
      it "calculates triangle intersection 3 points" $
         calcIntersecTriangle t11 2.0 `shouldBe` IntersecTriangle t11 [v1, v4, v6]
      it "calculates no triangle intersection" $
         calcIntersecTriangle t1 3.0 `shouldBe` IntersecTriangle t1 []
      it "calculates all triangle intersections" $
         map (`calcIntersecTriangle` 1.0) [t1,t2,t3,t4,t5,t6,t7,t8] `shouldBe` intTris
      it "calculates all triangle 2 point intersections" $
         filter (\intTri -> length (intTri & view intersections) == 2) (map (`calcIntersecTriangle` 1.0) boxMesh) `shouldBe` intTris

   describe "LibHslicer.Contour.findConnection" $ do
      it "finds connection no done" $
         findConnection intTri1 (remove intTri1 intTris) [] `shouldBe` Just intTri2
      it "finds connection midway" $
         findConnection intTri2 [intTri3, intTri4, intTri5, intTri6, intTri7, intTri8] [intTri1] `shouldBe` Just intTri5
      it "finds connection closing circle (no todo)" $
         findConnection (IntersecTriangle t3 [Vertex 0.5 0.0 1.0, Vertex 0.0 0.0 1.0]) [] [intTri1, intTri2, intTri5, intTri6, intTri7, intTri8, intTri4] `shouldBe` Just intTri1
      it "finds no connection" $
         findConnection intTri1 [intTri4, intTri5, intTri7, intTri8] [] `shouldBe` Nothing
      it "finds no connection single point" $
         findConnection intTri1 [] [] `shouldBe` Nothing

   describe "LibHslicer.Contour.createCoherentPath" $ do
      it "calculates closed path" $
         createCoherentPath intTris `shouldBe` reverse path
      it "calculates open path" $
         createCoherentPath [intTri1, intTri2, intTri6, intTri7, intTri5] `shouldBe` reverse [intTri1, intTri2, intTri5, intTri6, intTri7]
      it "calculates unconnected paths" $
         createCoherentPath [intTri1, intTri2, intTri6, intTri5, intTri3, intTri4, intTri8] `shouldBe` reverse [intTri1, intTri2, intTri5, intTri6, intTri3, intTri4, intTri8]

   describe "LibHslicer.Contour.generateContour" $ do
      it "generates contour closed path" $
         generateContour boxMesh 1.0 `shouldBe` [Right (Outer $ makeContourCCW contour)]

   describe "LibHslicer.Contour.calculateOffsetForPoint" $ do
      it "offsets a path midpoint by -1" $
         calculateOffsetForPoint (-1) (Vertex 0 0 0) (Vertex 10 0 0) (Vertex 10 10 0) `shouldBe` Vertex 9 1 0
      it "offsets a path midpoint by -2" $
         calculateOffsetForPoint (-2) (Vertex 0 0 0) (Vertex 10 0 0) (Vertex 10 10 0) `shouldBe` Vertex 8 2 0
      it "offsets a path midpoint by -2" $
         calculateOffsetForPoint (-2) (Vertex 10 0 0) (Vertex 10 10 0) (Vertex 0 10 0) `shouldBe` Vertex 8 8 0
      it "offsets a path midpoint by -2" $
         calculateOffsetForPoint (-2) (Vertex 10 10 0) (Vertex 0 10 0) (Vertex 0 0 0) `shouldBe` Vertex 2 8 0
      it "offsets a path midpoint by -1" $
         calculateOffsetForPoint (-1) (Vertex 10 10 0) (Vertex 0 10 0) (Vertex 0 0 0) `shouldBe` Vertex 1 9 0
      it "offsets a path midpoint 90 degrees on colinear segment" $
         calculateOffsetForPoint (-1) (Vertex 0 10 0) (Vertex 0 1 0) (Vertex 0 0 0) `shouldBe` Vertex 1 1 0
      it "offsets problematic path section by -1" $
         calculateOffsetForPoint (-1) (last $ init testpath1) (head testpath1) (testpath1!!1) `shouldBe` Vertex 1.000000 0.133344 0.200000

   describe "LibHslicer.Contour.calculateOffsetForContour" $ do
      it "offsets a contour by -1" $
         calculateOffsetForContour (-1) rectangle `shouldBe` rectangle_offset1
      it "offsets clockwise testpath by -1" $
         print $ calculateOffsetForContour (-1) $ makeContourCCW testpathcw

   describe "LibHslicer.Contour.downRightVertex" $ do
      it "gives back most down right vertex" $
         downRightVertex (Vertex 2 0 0) (Vertex 2 1 0) `shouldBe` Vertex 2 0 0

   describe "LibHslicer.Contour.oneVertexOnConvexHull" $ do   
      it "gives back one random point on convex hull of contour" $
         oneVertexOnConvexHull rectangle `shouldBe` Vertex 10 0 0
      it "gives back one random point on convex hull of contour" $
         oneVertexOnConvexHull rotatedrectangle `shouldBe` Vertex 10 0 0

   describe "LibHslicer.Contour.hasCCWWinding" $ do   
      it "Returns True if Contour has CCW Winding Direction" $
         hasCCWWinding rectangle `shouldBe` True
      it "Returns False if Contour has CW Winding Direction" $
         hasCCWWinding (reverse rectangle) `shouldBe` False
      it "Returns True if Contour with most down right point not in first spot has CCW Winding Direction" $
         hasCCWWinding rotatedrectangle `shouldBe` True
      it "Returns False if Contour with most down right point not in first spot has CW Winding Direction" $
         hasCCWWinding (reverse rotatedrectangle) `shouldBe` False
         
   describe "LibHslicer.Contour.makeContourCCW" $ do   
      it "CCW Winding Direction stays CCW" $
         makeContourCCW rectangle `shouldBe` rectangle
      it "CW Winding Direction of rectangle becomes CCW" $
         makeContourCCW (reverse rectangle) `shouldBe` rectangle
      it "CCW Winding Direction stays CCW" $
         makeContourCCW rotatedrectangle `shouldBe` rotatedrectangle
      it "CW Winding Direction of rectangle becomes CCW" $
         makeContourCCW (reverse rotatedrectangle) `shouldBe` rotatedrectangle