module LibGcode(
    GCmd (..),
    GArg (..),
    writeGCode,
    prettyGCode,
    preamble
)
    where

--import GHC.IO.FD (openFile)
--import GHC.IO.IOMode (IOMode(WriteMode))

preamble = "G21 ; set units to millimeters \n\
           \G90 ; use absolute coordinates \n\
           \M83 ; use relative distances for extrusion\n"

writeGCode :: FilePath -> [GCmd] -> IO()
writeGCode fp gcode = writeFile fp $ preamble ++ prettyGCode gcode

-- List of GCmd (GCode)
prettyGCode :: [GCmd] -> String
prettyGCode [] = ""
prettyGCode (x:xs) = prettyGCmd x ++ "\n" ++ prettyGCode xs

-- GCmd
data GCmd = AbsProgr [GArg]
            | RelProgr [GArg]
            | Text String deriving Show

prettyGCmd :: GCmd -> String
prettyGCmd (AbsProgr x) = "G1 " ++ prettyGArgs x
prettyGCmd (RelProgr x) = "G0 " ++ prettyGArgs x
prettyGCmd (Text x) = show x

-- GCmd Arguments
prettyGArgs :: [GArg] -> String
prettyGArgs [] = ""
prettyGArgs (x:xs) = prettyGArg x ++ " " ++ prettyGArgs xs

data GArg = GArg {name :: String, value :: Maybe String} deriving Show

prettyGArg :: GArg -> String
prettyGArg (GArg name Nothing) = name
prettyGArg (GArg name (Just value)) = name ++ value