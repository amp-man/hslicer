{-# LANGUAGE FlexibleInstances #-}
module ShowGcode
    where

import GHC.IO.FD (openFile)
import GHC.IO.IOMode (IOMode(WriteMode))

writeGcode :: FilePath -> [GCmd] -> IO()
writeGcode fp gcode = writeFile fp $ show gcode

-- List of GCmd (GCode)
instance {-# OVERLAPPING #-} Show [GCmd] where
    showsPrec _ x = showsGcode x

showsGcode :: [GCmd] -> ShowS
showsGcode [] = shows ""
showsGcode (x:xs) = shows x . ('\n':) . showsGcode xs

-- GCmd
data GCmd = AbsProgr [GArg]
            | RelProgr [GArg]
            | Text String

instance Show GCmd where
    show (AbsProgr x) = "G91 " ++ show x
    show (RelProgr x) = "G90 " ++ show x
    show (Text x) = show x

-- GCmd Arguments
instance {-# OVERLAPPING #-} Show [GArg] where
    show [] = ""
    show (x:xs) = show x ++ " " ++ show xs

data GArg = GArg {name :: String, value :: Maybe String}

instance Show GArg where
    show (GArg name Nothing) = show name
    show (GArg name (Just value)) = show name ++ show value