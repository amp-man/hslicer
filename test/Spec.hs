-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import LibGcode(GCmd(..), GArg(..), writeGCode, prettyGCode)

testgcode = [AbsProgr [GArg {name="X",value=Just "0"},GArg {name="Y",value=Just "1"}], AbsProgr [GArg {name="Y",value=Just "1"}]]

main :: IO ()
main = do
    print $ prettyGCode testgcode
    writeGCode "/Users/ludwig/home/Studium/Bachelor Informatik/7. Semester/FFP/Projekt/hslicer/test.gcode" testgcode