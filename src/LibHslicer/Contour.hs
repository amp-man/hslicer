{-# LANGUAGE TemplateHaskell #-}

module LibHslicer.Contour where

import TriangleMesh
import Control.Lens (over, view, set, (&), makeLenses)
import Data.Maybe (fromMaybe)
import Data.List
import Debug.Trace
import Control.Parallel.Strategies
import Control.DeepSeq

data IntersecTriangle = IntersecTriangle {_triangle :: Triangle, _intersections :: [Vertex]} deriving Show
newtype InnerContour = Inner [Vertex] deriving (Show, Eq)
newtype OuterContour = Outer [Vertex] deriving (Show, Eq)

instance Eq IntersecTriangle where
    (IntersecTriangle t ins) == (IntersecTriangle t' ins') = t == t' && all (`elem` ins') ins && all (`elem` ins) ins'

instance NFData InnerContour where
    rnf (Inner xs) = rnf xs

instance NFData OuterContour where
    rnf (Outer xs) = rnf xs

makeLenses ''IntersecTriangle

-- Checks whether the line between two specified vertices is intersecting the xy-plane at a specified slice height
-- Assuming planar slices, thus only checking for the value of the z-coordinate
isIntersectingVertex :: Vertex -> Vertex -> Double -> Bool
isIntersectingVertex v1 v2 zSlice = let z1 = view zCoord v1
                                        z2 = view zCoord v2
                                    in (z1 >= zSlice && z2 <= zSlice) || (z1 <= zSlice && z2 >= zSlice)

-- Checks whether a specified triangle is intersecting the xy-plane at a specified slice height
isIntersectingTriangle :: Triangle -> Double -> Bool
isIntersectingTriangle (Triangle v1 v2 v3) zSlice = isIntersectingVertex v1 v2 zSlice || isIntersectingVertex v1 v3 zSlice || isIntersectingVertex v2 v3 zSlice

-- Get intersections of two triangle sides (could be one or two or none)
calcIntersecVertex :: Vertex -> Vertex -> Double -> Maybe (Either (Vertex, Vertex) Vertex)
calcIntersecVertex v1 v2 zSlice = if isIntersectingVertex v1 v2 zSlice
    then Just $ calcIntersection v1 v2 zSlice
    else Nothing
    where
        calcIntersection v1 v2 zSlice = let zk = max (view zCoord v1) (view zCoord v2)
                                            zj = min (view zCoord v1) (view zCoord v2)
                                            mu = (zSlice - zj) / (zk - zj)
                                        in if zk == zj
                                            then Left (v1, v2)
                                            else if view zCoord v1 > view zCoord v2
                                                then Right $ mapV (*mu) v1 `addV` mapV (*(1-mu)) v2
                                                else Right $ mapV (*mu) v2 `addV` mapV (*(1-mu)) v1

-- Calculate intersections of a triangle at a certain z plane
calcIntersecTriangle :: Triangle -> Double -> IntersecTriangle
calcIntersecTriangle t zSlice = calcIntersections t zSlice [] where
    calcIntersections (Triangle v1 v2 v3) zSlice l =
        let l'   = case calcIntersecVertex v1 v2 zSlice of
                     Just (Left (v1, v2)) -> v1 : v2 : l
                     Just (Right v) -> v : l
                     Nothing -> l
            l''  = case calcIntersecVertex v1 v3 zSlice of
                     Just (Left (v1', v2')) -> v1' : v2' : l'
                     Just (Right v') -> v' : l'
                     Nothing -> l'
            l''' = case calcIntersecVertex v2 v3 zSlice of
                     Just (Left (v1'', v2'')) -> v1'' : v2'' : l''
                     Just (Right v'') -> v'' : l''
                     Nothing -> l''
        in IntersecTriangle (Triangle v1 v2 v3) (nub l''')

-- Create closed/coherent path by sorting the IntersecTriangles
createCoherentPath :: [IntersecTriangle] -> [IntersecTriangle]
createCoherentPath [] = []
createCoherentPath (t:ts) = createCoherentPath' t ts [] where
    createCoherentPath' curr [] done = let conn = findConnection curr [] done
                                       in case conn of
                                           Just next -> next : curr : done
                                           Nothing -> curr : done
    createCoherentPath' curr todo done = let conn = findConnection curr todo done
                                         in case conn of
                                             Just next -> if next `elem` todo
                                                 then createCoherentPath' next (remove next todo) (curr:done)
                                                 else createCoherentPath' (head todo) (tail todo) (next:curr:done)
                                             Nothing -> createCoherentPath' (head todo) (tail todo) (curr:done)

-- Find adjacent/touching triangles
findConnection :: IntersecTriangle -> [IntersecTriangle] -> [IntersecTriangle] -> Maybe IntersecTriangle
findConnection intTri [] [] = Nothing
findConnection intTri todo done =
    let start = last $ view intersections intTri
    in case find (\e -> start `elem` (e & view intersections)) todo of
        Just dest -> Just $ putConnectionFirst start dest
        Nothing -> find (\e -> start `elem` (e & view intersections)) done

-- Puts the specified Vertex at the beginning of the list of intersection points within the IntersecTriangle
-- Only if the IntersecTriangle's intersections already contain the specified Vertex
-- Ensures that recursion with findConnection is working correctly
putConnectionFirst :: Vertex -> IntersecTriangle -> IntersecTriangle
putConnectionFirst v intTri =
  if v `elem` view intersections intTri
    then intTri & set intersections (v : remove v (view intersections intTri))
    else error "The triangle must intersect the slicing plane at the connection vertex."

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove e xs = [x | x <- xs, x /= e]

-- Divide all triangles that intersect plane into groups of adjacent/touching triangles
-- Assuming that triangles are sorted next to each other
separatePaths :: [IntersecTriangle] -> [[IntersecTriangle]]
separatePaths ts = separatePaths' ts [] [] where
    separatePaths' [] [] akk = akk
    separatePaths' [] curr akk = curr : akk
    separatePaths' (x:xs) curr akk = if x `elem` curr
        then separatePaths' xs [] ((x : curr) : akk)
        else separatePaths' xs (x:curr) akk

-- Concatenate all intersection points of the triangles that intersect the plane
pathToContour :: [IntersecTriangle] -> [Vertex]
pathToContour [] = []
pathToContour xs = concatMap (\t -> init $ t & view intersections) xs

-- TODO: We need a different approach here to accommodate nested outer contours
isInnerContourOf :: [Vertex] -> [Vertex] -> Bool
isInnerContourOf [] _ = True
isInnerContourOf (x:xs) ref = any ((< view xCoord x) . view xCoord) ref
                           && any ((> view xCoord x) . view xCoord) ref
                           && any ((< view yCoord x) . view yCoord) ref
                           && any ((> view yCoord x) . view yCoord) ref

-- Checks if a contour is inside any other one
-- TODO: We need a different approach here to accommodate nested outer contours
isInnerContour :: [Vertex] -> [[Vertex]] -> Bool
isInnerContour _ [] = False
isInnerContour x cs = or [x `isInnerContourOf` c | c <- cs, x /= c]

-- Types a contour by checking isInnerContour
classifyContour :: [[Vertex]] -> [Either InnerContour OuterContour]
classifyContour cs = map (\ x -> if isInnerContour x cs then Left (Inner x) else Right (Outer x)) cs

-- Get the most down right vertex in xy plane of the two
downRightVertex :: Vertex -> Vertex -> Vertex
downRightVertex v1@(Vertex x1 y1 _) v2@(Vertex x2 y2 _) = if (y1 < y2)
                                                        || (y1 == y2 && x1 > x2) then v1 else v2

-- TODO: Eventually switch to -> Maybe Vertex instead of error
oneVertexOnConvexHull :: [Vertex] -> Vertex
oneVertexOnConvexHull [] = error "No Vertices in List"
oneVertexOnConvexHull (x:xs) = foldr downRightVertex x xs

-- Inspired by: http://www.faqs.org/faqs/graphics/algorithms-faq/ "How do I find the orientation of a simple polygon?"
hasCCWWinding :: [Vertex] -> Bool
hasCCWWinding c = hasCCWWinding' pointOnConvexHull c
    where
        pointOnConvexHull = oneVertexOnConvexHull c
        hasCCWWinding' :: Vertex -> [Vertex] -> Bool
        hasCCWWinding' _ [] = False
        hasCCWWinding' chv (p1:p2:p3:ps) = if p2 == chv
                                            then xyCrossProduct (p1 `addV` vertexFlip p2) (p3 `addV` vertexFlip p2) < 0
                                            else hasCCWWinding' chv (p2:p3:ps++[p2])
        hasCCWWinding' _ _ = False

makeContourCCW :: [Vertex] -> [Vertex]
makeContourCCW c = if hasCCWWinding c then c else reverse c

-- One contour from a triangle mesh at slice height zSlice
generateContour :: [Triangle] -> Double -> [Either InnerContour OuterContour]
generateContour [] _ = []
generateContour ts zSlice = classifyContour $ map (makeContourCCW.pathToContour) (separatePaths $ createCoherentPath $
    filter (\intTri -> length (intTri & view intersections) == 2) (map (`calcIntersecTriangle` zSlice) ts))

-- Inspired by Aichholzer et al.(1995),"A novel type of skeleton for polygons": https://www.jucs.org/jucs_1_12/a_novel_type_of/
-- Assuming counter-clockwise winding of path: Left is inside of contour, right outside
-- Offset point is calculated by moving contour point along diagonal of two contour vertices
-- Negative offset is to inside of contour, positive to outside
calculateOffsetForPoint :: Double -> Vertex -> Vertex -> Vertex -> Vertex
calculateOffsetForPoint a p1 p2 p3 = p2 `addV` mapV (*diagoffset) offsetnormal
    where
        p1p2vec = p2 `addV` vertexFlip p1
        p2p3vec = p3 `addV` vertexFlip p2
        offsetnormal = offsetNormal p1p2vec p2p3vec
        alpha = acos(dotProduct p1p2vec offsetnormal / (vertexLength p1p2vec * vertexLength offsetnormal))
        b = a / tan alpha
        diagoffset = signum a * sqrt(a**2 + b**2)

-- Offsets all points in a Contour ([Vertex])
-- First Point is contained at beginning and end of Contour
-- TODO: Eventually switch to circular data structure
-- Removes unnecessary middle points of a path segment
-- TODO: Detect if offset crosses straight skeleton and eliminate point if so
calculateOffsetForContour :: Double -> [Vertex] -> [Vertex]
calculateOffsetForContour o c@(p1:p2:ps) = let offsetp1 = calculateOffsetForPoint o (last $ init ps) p1 p2 
                                           in offsetp1 : calculateOffsetForContour' o c ++ [offsetp1]
      where calculateOffsetForContour' :: Double -> [Vertex] -> [Vertex]
            calculateOffsetForContour' _ [] = []
            calculateOffsetForContour' o (p1:p2:p3:ps) = if areColinear p1 p2 p3 then psfunc else calculateOffsetForPoint o p1 p2 p3 : psfunc
                where psfunc = calculateOffsetForContour' o (p2:p3:ps)
            calculateOffsetForContour' _ _ = []
calculateOffsetForContour _ _ = []

-- Flip offset direction to stay inside the objects' contours
calculateOffsetInnerOuter :: Double -> Either InnerContour OuterContour -> [Vertex]
calculateOffsetInnerOuter o (Left (Inner c)) = calculateOffsetForContour (o*(-1)) c
calculateOffsetInnerOuter o (Right (Outer c)) = calculateOffsetForContour o c