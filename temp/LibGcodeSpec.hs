import LibGcode(GCmd(..), GArg(..))

testgcode = [AbsProgr [GArg {name="X",value="0"}], AbsProgr [GArg {name="Y",value="1"}]]

main :: IO ()
main = print testgcode