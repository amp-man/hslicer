{-# LANGUAGE FlexibleInstances #-}
module LibGcode
    where

data GCmd = AbsProgr Coordinates
            | RelProgr Coordinates
            | Text String

data Coordinates = Coord {x :: Double, y :: Double} deriving Show
-- data Coordinates = Coord {x :: Double, y :: Double, z :: Double, e :: Double, f :: Double} deriving Show

-- instance Show Coordinates where
--     show (AbsProgr x) = "G91 " ++ show x
--     show (RelProgr x) = "G90 " ++ show x
--     show (Text x) = show x

instance Show GCmd where
    show (AbsProgr x) = "G91 " ++ show x
    show (RelProgr x) = "G90 " ++ show x
    show (Text x) = show x

showsGcode :: [GCmd] -> ShowS
showsGcode [] = shows ""
showsGcode (x:xs) = shows x . ('\n':) . showsGcode xs

instance {-# OVERLAPPING #-} Show [GCmd] where
    showsPrec _ x = showsGcode x