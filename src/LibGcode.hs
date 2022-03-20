module LibGcode(
    GCmd (..),
    GArg (..),
    writeGCode,
    prettyGCode,
    preamble
)
    where

-- Kept flexible by using String and Maybe String for an optional value
-- TODO: Eventually move to a more defined data structure, or switch to a bnf form language description
data GArg = GArg {name :: String, value :: Maybe String} deriving Show

data GCmd = AbsProgr [GArg]
            | RelProgr [GArg]
            | Text String deriving Show

preamble = "G21 ; set units to millimeters \n\
           \G90 ; use absolute coordinates \n\
           \M83 ; use relative distances for extrusion\n"

writeGCode :: FilePath -> [GCmd] -> IO()
writeGCode fp gcode = writeFile fp $ preamble ++ prettyGCode gcode

prettyGArg :: GArg -> String
prettyGArg (GArg name Nothing) = name
prettyGArg (GArg name (Just value)) = name ++ value

prettyGArgs :: [GArg] -> String
prettyGArgs [] = ""
prettyGArgs (x:xs) = prettyGArg x ++ " " ++ prettyGArgs xs

prettyGCmd :: GCmd -> String
prettyGCmd (AbsProgr x) = "G1 " ++ prettyGArgs x
prettyGCmd (RelProgr x) = "G0 " ++ prettyGArgs x
prettyGCmd (Text x) = show x

prettyGCode :: [GCmd] -> String
prettyGCode [] = ""
prettyGCode (x:xs) = prettyGCmd x ++ "\n" ++ prettyGCode xs