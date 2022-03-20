module LibHslicer.PlanarSliceSpec where

import LibHslicer.PlanarSlice
import LibHslicer.Contour
import TriangleMesh
import LibGcode
import Test.Hspec (Spec, describe, context, it, shouldBe)
import Examples.Box as B

spec :: Spec
spec = do
   describe "LibHslicer.PlanarSlice.calcExtrVol" $ do
      it "Calculates extrusion volume different vertices" $
         calcExtrVol B.v1 B.v2 sParamsDefault `shouldBe` (sqrt 5 * 0.4 * 0.2, "mm3")
      it "Calculates extrusion volume same vertex" $
         calcExtrVol B.v1 B.v1 sParamsDefault `shouldBe` (0.0, "mm3")
   
   describe "LibHslicer.PlanarSlice.calcPParams" $ do
      it "Calculates print parameters for adjacent vertices" $
         calcPParams B.v1 B.v2 sParamsDefault `shouldBe` PParams {_extMove = ((sqrt 5 * 0.4 * 0.2)/(pi * (1.75 / 2) ** 2), "mm"), _velocity = _speed sParamsDefault}
      it "Calculates print parameters for same vertex" $
         calcPParams B.v1 B.v1 sParamsDefault `shouldBe` PParams {_extMove = (0.0, "mm"), _velocity = _speed sParamsDefault}

   describe "LibHslicer.PlanarSlice.printPrep" $ do
      it "Prepares contour for printing" $
         printPrep [B.contour] sParamsDefault `shouldBe` [B.contour1Combination] 
      it "Prepares bigger contour for printing" $
         printPrep [map (mapV (*2)) B.contour] sParamsDefault `shouldBe` [B.contour1_x2Combination]