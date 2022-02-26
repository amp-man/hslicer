import LibGcode(GCmd(..), Coordinates(..))

testgcode = [AbsProgr Coord {x=0, y=0}, AbsProgr Coord {x=1,y=1}]

main :: IO ()
main = print testgcode