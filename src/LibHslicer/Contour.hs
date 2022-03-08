{-# LANGUAGE TemplateHaskell #-}

module LibHslicer.Contour where

import Lib3mf
import Control.Lens (over, view, set, (&), makeLenses)
import Data.Maybe (fromMaybe)
import Data.List

data IntersecTriangle = IntersecTriangle {_triangle :: Triangle, _intersections :: [Vertex]} deriving Show
newtype InnerContour = Inner [Vertex] deriving (Show, Eq)
newtype OuterContour = Outer [Vertex] deriving (Show, Eq)

instance Eq IntersecTriangle where
    (IntersecTriangle t ins) == (IntersecTriangle t' ins') = t == t' && all (`elem` ins') ins && all (`elem` ins) ins'

makeLenses ''IntersecTriangle

isIntersectingVertex :: Vertex -> Vertex -> Double -> Bool
isIntersectingVertex v1 v2 zSlice = let z1 = view zCoord v1
                                        z2 = view zCoord v2
                                    in (z1 >= zSlice && z2 <= zSlice) || (z1 <= zSlice && z2 >= zSlice)

isIntersectingTriangle :: Triangle -> Double -> Bool
isIntersectingTriangle (Triangle v1 v2 v3) zSlice = isIntersectingVertex v1 v2 zSlice || isIntersectingVertex v1 v3 zSlice || isIntersectingVertex v2 v3 zSlice

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

-- calcIntersecTriangles :: [Triangle] -> Double -> [IntersecTriangle]
-- calcIntersecTriangles = undefined

putConnectionFirst :: Vertex -> IntersecTriangle -> IntersecTriangle
putConnectionFirst v intTri = if v `elem` view intersections intTri
    then intTri & set intersections (v : remove v (view intersections intTri))
    else error "The triangle must intersect the slicing plane at the connection vertex."

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove e xs = [x | x <- xs, x /= e]

-- Geschlossenen Pfad durch das Sortieren der InterSecTriangles erzeugen
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

findConnection :: IntersecTriangle -> [IntersecTriangle] -> [IntersecTriangle] -> Maybe IntersecTriangle
findConnection intTri [] [] = Nothing
findConnection intTri todo done =
    let start = last $ view intersections intTri
    in case find (\e -> start `elem` (e & view intersections)) todo of
        Just dest -> Just $ putConnectionFirst start dest
        Nothing -> find (\e -> start `elem` (e & view intersections)) done

separatePaths :: [IntersecTriangle] -> [[IntersecTriangle]]
separatePaths ts = separatePaths' ts [] [] where
    separatePaths' [] [] akk = akk
    separatePaths' [] curr akk = curr : akk
    separatePaths' (x:xs) curr akk = if x `elem` curr
        then separatePaths' xs [] ((x : curr) : akk)
        else separatePaths' xs (x:curr) akk

pathToContour :: [IntersecTriangle] -> [Vertex]
pathToContour = concatMap (\t -> init $ t & view intersections)

isInnerContourOf :: [Vertex] -> [Vertex] -> Bool
isInnerContourOf [] _ = True
isInnerContourOf (x:xs) ref = any ((< view xCoord x) . view xCoord) ref 
                           && any ((> view xCoord x) . view xCoord) ref 
                           && any ((< view yCoord x) . view yCoord) ref
                           && any ((> view yCoord x) . view yCoord) ref

isInnerContour :: [Vertex] -> [[Vertex]] -> Bool
isInnerContour _ [] = False
isInnerContour x cs = or [x `isInnerContourOf` c | c <- cs, x /= c]

classifyContour :: [[Vertex]] -> [Either InnerContour OuterContour]
classifyContour cs = map (\ x -> if isInnerContour x cs then Left (Inner x) else Right (Outer x)) cs

-- [Triangle] --> filtere intersecting triangles => [Triangle] (length <= input list) --> map calcIntersecTriangle => [IntersecTriangle] => ordnen

generateContour :: [Triangle] -> Double -> [Either InnerContour OuterContour]
generateContour [] _ = []
generateContour ts zSlice = classifyContour $ map pathToContour (separatePaths $ createCoherentPath $ 
    filter (\intTri -> length (intTri & view intersections) == 2) (map (`calcIntersecTriangle` zSlice) ts))

-----------------------------------------------------------------
--Contour Fulfilment : Layer Width, Layer Height als param in main

calculateOffset :: [Either InnerContour OuterContour] -> Double
calculateOffset = undefined

--                             main: LineWidth, TriangleMesh -> [Contour]
--                         /                                                               \
-- LineWidth, TriangleMesh                                                                   \
--                         \                                                                  generateGCode -> G91 X0 Y0 G91 X1 Y1 E0.75