module LibGcodeSpec where

import LibGcode(GCmd, GArg, writeGCode, prettyGCode)

import Test.Hspec (Spec, describe, context, it, shouldBe)

spec :: Spec
spec = do
    let gcode = [AbsProgr [GArg {name="X",value=Just "0"},GArg {name="Y",value=Just "1"}], AbsProgr [GArg {name="Y",value=Just "1"}]]
    let gcoderesult = "G91 X0 Y1 \nG91 Y1 \n"
    let filepath = "/Users/ludwig/home/Studium/Bachelor Informatik/7. Semester/FFP/Projekt/hslicer/test/test.gcode"
    it "prettifies GCode" $
        prettyGCode gcode `shouldBe` gcoderesult
    it "writes GCode file" $
        writeGCode filepath gcode