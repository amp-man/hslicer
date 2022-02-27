module LibHslicer where

import Lib3mf

data IntersecTriangle = IntersecTriangle {_triangle :: Triangle, _intersections :: [Vertex]}
newtype Contour = Contour [Vertex]

isIntersectingVertex :: Vertex -> Vertex -> Double -> Bool
isIntersectingVertex = undefined

-- isIntersectingVertex muss in mindestens einem Fall True sein
isIntersectingTriangle :: Triangle -> Double -> Bool
isIntersectingTriangle = undefined

calcIntersecVertex :: Vertex -> Vertex -> Double -> Maybe Vertex 
calcIntersecVertex = undefined

calcIntersecTriangle :: Triangle -> Double -> IntersecTriangle
calcIntersecTriangle = undefined

-- calcIntersecTriangles :: [Triangle] -> Double -> [IntersecTriangle]
-- calcIntersecTriangles = undefined

-- Geschlossenen Pfad durch das Sortieren der InterSecTriangles erzeugen
createCoherentPath :: [IntersecTriangle] -> [IntersecTriangle]
createCoherentPath = undefined

-- [Triangle] --> filtere intersecting triangles => [Triangle] (length <= input list) --> map calcIntersecTriangle => [IntersecTriangle] => ordnen
-- Fall1: Kein Cluster
-- Fall2: Cluster (3 IntersectingVertices): NOCH NICHT Implementiert
generateContour :: [Triangle] -> Contour
generateContour = undefined

--                             main: LineWidth, TriangleMesh -> [Contour]
--                         /                                                               \
-- LineWidth, TriangleMesh                                                                   \
--                         \                                                                  generateGCode -> G91 X0 Y0 G91 X1 Y1 E0.75