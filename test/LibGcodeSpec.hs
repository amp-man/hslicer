module LibGcodeSpec where

import LibGcode

import Test.Hspec (Spec, describe, context, it, shouldBe)

spec :: Spec
spec = do
   let gcode = [AbsProgr [GArg {name="X",value=Just "0"},GArg {name="Y",value=Just "1"}], AbsProgr [GArg {name="Y",value=Just "1"}]]
   let gcoderesult = "G1 X0 Y1 \nG1 Y1 \n"
   let testfilepath = "test/LibGcodeSpec_res/test.gcode"
   let specfilepath = "test/LibGcodeSpec_res/testspec.gcode"
   
   it "prettifies GCode" $
      prettyGCode gcode `shouldBe` gcoderesult

   it "writes GCode file" $ do
      writeGCode testfilepath gcode
      testfile <- readFile testfilepath
      specfile <- readFile specfilepath
      testfile `shouldBe` specfile