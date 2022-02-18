import Lib

main :: IO ()
--main = putStrLn "Test suite not yet implemented"
main = someFunc

vertex_0 :: Vertex
vertex_0 = Vertex 0.000 0.000 0.000

-- <mesh>
--         <vertices>
--           <vertex x="0.000" y="0.000" z="0.000" />
--           <vertex x="0.000" y="20.207" z="0.000" />
--           <vertex x="10.104" y="20.207" z="0.000" />
--           <vertex x="10.104" y="0.000" z="0.000" />
--           <vertex x="0.000" y="0.000" z="30.308" />
--           <vertex x="0.000" y="20.207" z="30.308" />
--           <vertex x="10.104" y="20.207" z="30.308" />
--           <vertex x="10.104" y="0.000" z="30.308" />
--         </vertices>
--         <triangles>
--           <triangle v1="0" v2="1" v3="2" />
--           <triangle v1="0" v2="2" v3="3" />
--           <triangle v1="4" v2="7" v3="6" />
--           <triangle v1="4" v2="6" v3="5" />
--           <triangle v1="0" v2="4" v3="5" />
--           <triangle v1="0" v2="5" v3="1" />
--           <triangle v1="1" v2="5" v3="6" />
--           <triangle v1="1" v2="6" v3="2" />
--           <triangle v1="2" v2="6" v3="7" />
--           <triangle v1="2" v2="7" v3="3" />
--           <triangle v1="3" v2="7" v3="4" />
--           <triangle v1="3" v2="4" v3="0" />
--         </triangles>
--       </mesh>