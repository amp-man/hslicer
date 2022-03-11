module LibHslicer.PlanarSliceSpec where

import LibHslicer.PlanarSlice
import LibHslicer.Contour
import TriangleMesh
import Test.Hspec (Spec, describe, context, it, shouldBe)

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
        contour = Right (Outer [Vertex 0.0 0.0 1.0, Vertex 0.0 0.5 1.0, Vertex 0.0 1.0 1.0, Vertex 0.5 1.0 1.0, Vertex 1.0 1.0 1.0, Vertex 1.0 0.5 1.0, Vertex 1.0 0.0 1.0, Vertex 0.5 0.0 1.0, Vertex 0.0 0.0 1.0])

    describe "LibHslicer.PlanarSlice.sliceContours" $ do
       it "Slices a Mesh with 0.2 layer height" $ do
          print $ sliceContours mesh sParamsDefault