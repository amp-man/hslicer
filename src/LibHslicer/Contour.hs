{-# LANGUAGE TemplateHaskell #-}

module LibHslicer.Contour where

import Lib3mf
import Control.Lens (over, view, set, (&), makeLenses)
import Data.Maybe (fromMaybe)
import Data.List

data IntersecTriangle = IntersecTriangle {_triangle :: Triangle, _intersections :: [Vertex]} deriving Show
newtype InnerContour = Inner [Vertex]
newtype OuterContour = Outer [Vertex]

instance Eq IntersecTriangle where
    (IntersecTriangle t ins) == (IntersecTriangle t' ins') = t == t' && all (`elem` ins') ins && all (`elem` ins) ins'

makeLenses ''IntersecTriangle

isIntersectingVertex :: Vertex -> Vertex -> Double -> Bool
isIntersectingVertex v1 v2 zSlice = let z1 = view zCoord v1
                                        z2 = view zCoord v2
                                    in (z1 >= zSlice && z2 <= zSlice) || (z1 <= zSlice && z2 >= zSlice)

-- isIntersectingVertex muss in mindestens einem Fall True sein
isIntersectingTriangle :: Triangle -> Double -> Bool
isIntersectingTriangle (Triangle v1 v2 v3) zSlice = isIntersectingVertex v1 v2 zSlice || isIntersectingVertex v1 v3 zSlice || isIntersectingVertex v2 v3 zSlice

calcIntersecVertex :: Vertex -> Vertex -> Double -> Maybe Vertex
calcIntersecVertex v1 v2 zSlice = if isIntersectingVertex v1 v2 zSlice
    then Just $ calcIntersection v1 v2 zSlice
    else Nothing
    where
        calcIntersection v1 v2 zSlice = let zk = max (view zCoord v1) (view zCoord v2)
                                            zj = min (view zCoord v1) (view zCoord v2)
                                            mu = (zSlice - zj) / (zk - zj)
                                        in if view zCoord v1 > view zCoord v2
                                            then mapV (*mu) v1 `addV` mapV (*(1-mu)) v2
                                            else mapV (*mu) v2 `addV` mapV (*(1-mu)) v1

calcIntersecTriangle :: Triangle -> Double -> IntersecTriangle
calcIntersecTriangle t zSlice = calcIntersections t zSlice [] where
    calcIntersections (Triangle v1 v2 v3) zSlice l =
        let l'   = case calcIntersecVertex v1 v2 zSlice of
                     Just v -> v : l
                     Nothing -> l
            l''  = case calcIntersecVertex v1 v3 zSlice of
                     Just v' -> v' : l'
                     Nothing -> l'
            l''' = case calcIntersecVertex v2 v3 zSlice of
                     Just v'' -> v'' : l''
                     Nothing -> l''
        in IntersecTriangle (Triangle v1 v2 v3) l'''

-- calcIntersecTriangles :: [Triangle] -> Double -> [IntersecTriangle]
-- calcIntersecTriangles = undefined

putConnectionFirst :: Vertex -> IntersecTriangle -> IntersecTriangle
putConnectionFirst v intTri = intTri & set intersections (v : remove v (view intersections intTri)) where
    remove _ [] = []
    remove e xs = [x | x <- xs, x /= e]

-- Geschlossenen Pfad durch das Sortieren der InterSecTriangles erzeugen
createCoherentPath :: [IntersecTriangle] -> [IntersecTriangle]
createCoherentPath intTris = createCoherentPath' intTris [] where
    createCoherentPath' [] done = done
    createCoherentPath' (t:ts) [] = createCoherentPath' ts [findConnection t ts [], t]
    createCoherentPath' todo done = createCoherentPath' (tail todo) (findConnection (head done) todo done : done)

findConnection :: IntersecTriangle -> [IntersecTriangle] -> [IntersecTriangle] -> IntersecTriangle
findConnection intTri [] [] = intTri
findConnection intTri todo done =
    let start = last $ view intersections intTri
    in case find (\e -> start `elem` (e & view intersections)) todo of
        Just dest -> putConnectionFirst start dest
        Nothing -> fromMaybe intTri (find (\e -> start `elem` (e & view intersections)) done)
{- findConnection intTri todo done = if any $ map (elem (last $ view intersections intTri)) (view traverse.intersections todo) 
    then find (\e -> elem (last $ view intersections intTri) (e & view intersections))-- get matching intTri from todo
    else if any $ map (elem (last $ view intersections intTri)) (done.traverse.intersections)
        then intTri -- get matching intTri from done
        else intTri -}

-- [Triangle] --> filtere intersecting triangles => [Triangle] (length <= input list) --> map calcIntersecTriangle => [IntersecTriangle] => ordnen
-- Fall1: Kein Cluster
-- Fall2: Cluster (3 IntersectingVertices): NOCH NICHT Implementiert
-- generateContour :: [Triangle] -> [Either InnerContour OuterContour]
-- generateContour = undefined

-- Assuming anti-clockwise winding of path: Left is inside of contour, right outside
-- Offset Point is calculated by moving contour point along diagonal of two contour vertices
-- Negative offset is to inside of contour, Positive to outside
-- TODO: All Vertices should be Points with 2 Dimensions only
calculateOffsetForPoint :: Double -> Vertex -> Vertex -> Vertex -> Vertex
calculateOffsetForPoint a p1 p2 p3 = p2 `addV` mapV (*diagoffset) offsetnormal
    where
        p1p2vec = p2 `addV` vertexFlip p1
        p2p3vec = p3 `addV` vertexFlip p2
        offsetnormal = offsetNormal p1p2vec p2p3vec
        alpha = acos(dot p1p2vec offsetnormal / (vertexLength p1p2vec * vertexLength offsetnormal))
        b = a / tan alpha
        diagoffset = signum a * sqrt(a**2 + b**2)

-- TODO: All Vertices should be Points with 2 Dimensions only
calculateOffsetForContour :: Double -> [Vertex] -> [Vertex]
calculateOffsetForContour _ [] = undefined
calculateOffsetForContour o (p1:p2:p3:ps) = calculateOffsetForPoint o p1 p2 p3 : calculateOffsetForContour o (p2:p3:ps)
calculateOffsetForContour o (p1:p2:ps) = undefined
calculateOffsetForContour o (p1:ps) = undefined