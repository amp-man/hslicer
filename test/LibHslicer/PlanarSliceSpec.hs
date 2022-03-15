module LibHslicer.PlanarSliceSpec where

import LibHslicer.PlanarSlice
import LibHslicer.Contour
import TriangleMesh
import Test.Hspec (Spec, describe, context, it, shouldBe)
import Examples.Box

spec :: Spec
spec = do
    describe "LibHslicer.PlanarSlice.sliceContours" $ do
       it "Slices a Mesh with 0.2 layer height" $ do
          print $ sliceContours boxMesh sParamsDefault