module LibGcode(
    GCmd,
    GArg,
    writeGCode,
    prettyGCode
)
    where

--import GHC.IO.FD (openFile)
--import GHC.IO.IOMode (IOMode(WriteMode))

writeGCode :: FilePath -> [GCmd] -> IO()
writeGCode fp gcode = writeFile fp $ prettyGCode gcode

-- List of GCmd (GCode)
prettyGCode :: [GCmd] -> String
prettyGCode [] = ""
prettyGCode (x:xs) = prettyGCmd x ++ "\n" ++ prettyGCode xs

-- GCmd
data GCmd = AbsProgr [GArg]
            | RelProgr [GArg]
            | Text String

prettyGCmd :: GCmd -> String
prettyGCmd (AbsProgr x) = "G91 " ++ prettyGArgs x
prettyGCmd (RelProgr x) = "G90 " ++ prettyGArgs x
prettyGCmd (Text x) = show x

-- GCmd Arguments
prettyGArgs :: [GArg] -> String
prettyGArgs [] = ""
prettyGArgs (x:xs) = prettyGArg x ++ " " ++ prettyGArgs xs

data GArg = GArg {name :: String, value :: Maybe String}

prettyGArg :: GArg -> String
prettyGArg (GArg name Nothing) = name
prettyGArg (GArg name (Just value)) = name ++ value