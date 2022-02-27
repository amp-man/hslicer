module LibHslicerSpec where

import LibHslicer
import Lib3mf

import Test.Hspec (Spec, describe, context, it, shouldBe)

spec :: Spec
spec = do
    let path = "test/Lib3mfSpec_res/box_sliced/3D/3dmodel.model"
    it "slices a [triangle]" $ do

vertex_0, vertex_1, vertex_2, vertex_3, vertex_4, vertex_5, vertex_6, vertex_7 :: Vertex
vertex_0 = Vertex {_x = 0.000, _y = 0.000, _z = 0.000}
vertex_1 = Vertex {_x = 0.000, _y = 20.207, _z = 0.000}
vertex_2 = Vertex {_x = 10.104, _y = 20.207, _z = 0.000}
vertex_3 = Vertex {_x = 10.104, _y = 0.000, _z = 0.000}
vertex_4 = Vertex {_x = 0.000, _y = 0.000, _z = 30.308}
vertex_5 = Vertex {_x = 0.000, _y = 20.207, _z = 30.308}
vertex_6 = Vertex {_x = 10.104, _y = 20.207, _z = 30.308}
vertex_7 = Vertex {_x = 10.104, _y = 0.000, _z = 30.308}

triangle_0, triangle_1, triangle_2, triangle_3, triangle_4, triangle_5, triangle_6, triangle_7, triangle_8, triangle_9, triangle_10, triangle_11 :: Triangle 
triangle_0 = Triangle vertex_0 vertex_1 vertex_2 -- <triangle v1="0" v2="1" v3="2" />
triangle_1 = Triangle vertex_0 vertex_2 vertex_3 -- <triangle v1="0" v2="2" v3="3" />
triangle_2 = Triangle vertex_4 vertex_7 vertex_6 -- <triangle v1="4" v2="7" v3="6" />
triangle_3 = Triangle vertex_4 vertex_6 vertex_5 -- <triangle v1="4" v2="6" v3="5" />
triangle_4 = Triangle vertex_0 vertex_4 vertex_5 -- <triangle v1="0" v2="4" v3="5" />
triangle_5 = Triangle vertex_0 vertex_5 vertex_1 -- <triangle v1="0" v2="5" v3="1" />
triangle_6 = Triangle vertex_1 vertex_5 vertex_6 -- <triangle v1="1" v2="5" v3="6" />
triangle_7 = Triangle vertex_1 vertex_6 vertex_2 -- <triangle v1="1" v2="6" v3="2" />
triangle_8 = Triangle vertex_2 vertex_6 vertex_7 -- <triangle v1="2" v2="6" v3="7" />
triangle_9 = Triangle vertex_2 vertex_7 vertex_3 -- <triangle v1="2" v2="7" v3="3" />
triangle_10 = Triangle vertex_3 vertex_7 vertex_4-- <triangle v1="3" v2="7" v3="4" />
triangle_11 = Triangle vertex_3 vertex_4 vertex_0-- <triangle v1="3" v2="4" v3="0" />

mesh = [triangle_0, triangle_1, triangle_2, triangle_3, triangle_4, triangle_5, triangle_6, triangle_7, triangle_8, triangle_9, triangle_10, triangle_11]