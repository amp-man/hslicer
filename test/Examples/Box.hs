module Examples.Box where

import TriangleMesh
import LibHslicer.Contour

-- vertices
v1, v2, v3, v4, v5, v6, v7, v8 :: Vertex
v1 = Vertex 0.0 0.0 2.0
v2 = Vertex 0.0 1.0 0.0
v3 = Vertex 0.0 0.0 0.0
v4 = Vertex 0.0 1.0 2.0
v5 = Vertex 1.0 1.0 0.0
v6 = Vertex 1.0 1.0 2.0
v7 = Vertex 1.0 0.0 0.0
v8 = Vertex 1.0 0.0 2.0

-- triangles
t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12 :: Triangle
t1 = Triangle v1 v2 v3
t2 = Triangle v1 v2 v4
t3 = Triangle v1 v3 v7
t4 = Triangle v1 v7 v8
t5 = Triangle v2 v4 v5
t6 = Triangle v4 v5 v6
t7 = Triangle v5 v6 v7
t8 = Triangle v6 v7 v8
t9 = Triangle v2 v3 v5
t10 = Triangle v3 v5 v7
t11 = Triangle v1 v4 v6
t12 = Triangle v1 v6 v8

-- mesh
boxMesh :: [Triangle]
boxMesh = [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12]

-- intersecting triangles at zSlice = 1.0
intTri1, intTri2, intTri3, intTri4, intTri5, intTri6, intTri7, intTri8 :: IntersecTriangle
intTri1 = IntersecTriangle t1 [Vertex 0.0 0.0 1.0, Vertex 0.0 0.5 1.0] -- zSlice = 1.0
intTri2 = IntersecTriangle t2 [Vertex 0.0 0.5 1.0, Vertex 0.0 1.0 1.0] -- zSlice = 1.0
intTri3 = IntersecTriangle t3 [Vertex 0.0 0.0 1.0, Vertex 0.5 0.0 1.0] -- zSlice = 1.0
intTri4 = IntersecTriangle t4 [Vertex 0.5 0.0 1.0, Vertex 1.0 0.0 1.0] -- zSlice = 1.0
intTri5 = IntersecTriangle t5 [Vertex 0.0 1.0 1.0, Vertex 0.5 1.0 1.0] -- zSlice = 1.0
intTri6 = IntersecTriangle t6 [Vertex 0.5 1.0 1.0, Vertex 1.0 1.0 1.0] -- zSlice = 1.0
intTri7 = IntersecTriangle t7 [Vertex 1.0 1.0 1.0, Vertex 1.0 0.5 1.0] -- zSlice = 1.0
intTri8 = IntersecTriangle t8 [Vertex 1.0 0.5 1.0, Vertex 1.0 0.0 1.0] -- zSlice = 1.0

intTris :: [IntersecTriangle]
intTris = [intTri1, intTri2, intTri3, intTri4, intTri5, intTri6, intTri7, intTri8]

-- calculation results
path :: [IntersecTriangle]
path = [intTri1, intTri2, intTri5, intTri6, intTri7, intTri8, intTri4, intTri3, intTri1]

contour :: Either a OuterContour
contour = Right (Outer [Vertex 0.0 0.0 1.0, Vertex 0.0 0.5 1.0, Vertex 0.0 1.0 1.0, Vertex 0.5 1.0 1.0, Vertex 1.0 1.0 1.0, Vertex 1.0 0.5 1.0, Vertex 1.0 0.0 1.0, Vertex 0.5 0.0 1.0, Vertex 0.0 0.0 1.0])