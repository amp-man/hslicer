{-# LANGUAGE TemplateHaskell #-}

module LibHslicer.PlanarSlice where

import TriangleMesh
import LibGcode
import LibHslicer.Contour
import Control.Lens
import Control.Parallel
import Numeric (showFFloat)
import Control.Parallel.Strategies
import Control.DeepSeq

-- We want to make sure that all dimensions stay in relation
-- by specifying a physical unit (e.g. mm, mm2, mm3 | cm, cm2, cm3)
-- TODO: Make sure to check for matching units
type Unit = (Double, String)

-- Parameters for the slice session
data NozzleAttrib = NAttrib { _nozzleWidth, _maxExtrusionAmount::Unit} deriving Show
data SliceParams = SParams { _sliceHeight::Unit, _contourLines::Int, _speed::Unit, _filamentWidth::Unit, _nozzleAttributes::NozzleAttrib } deriving Show
nAttribDefault = NAttrib { _nozzleWidth = (0.4,"mm"), _maxExtrusionAmount = (0.2,"mm3/s")}
sParamsDefault = SParams { _sliceHeight = (0.2,"mm"), _contourLines = 4, _speed = (20,"mm/s"), _filamentWidth = (1.75,"mm"), _nozzleAttributes = nAttribDefault }

-- Parameters per Contour Segment
data PrintParams = PParams {_extMove, _velocity :: Unit} deriving (Eq, Show)

data Combination = Comb {_position :: Vertex, _physics :: PrintParams} deriving (Show, Eq)

instance NFData Combination where
    rnf (Comb pos phys) = rnf pos `seq` rnf phys

instance NFData PrintParams where
    rnf (PParams e v) = rnf e `seq` rnf v

makeLenses ''SliceParams
makeLenses ''NozzleAttrib
makeLenses ''PrintParams
makeLenses ''Combination

-- Entry Function to slice a Triangle Mesh planarly
sliceMesh :: [Triangle] -> SliceParams -> [GCmd]
sliceMesh m sp = toGCmd $ printPrep (concatMap (calcMultiOffset sp) (sliceContours m sp)) sp

-- Calculate offsets with spacing as specified in SliceParams
-- by _contourLines and _nozzleWidth
calcMultiOffset :: SliceParams -> [Either InnerContour OuterContour] -> [[Vertex]]
calcMultiOffset sp cs = let fc = sp^.nozzleAttributes.nozzleWidth._1 / 2
                            lc = fc + (fromIntegral (sp^.contourLines - 1) * sp^.nozzleAttributes.nozzleWidth._1)
                            offsets = reverse $ map (* (-1)) (calcSliceOffsets fc (sp^.nozzleAttributes.nozzleWidth._1) lc)
                            offsetFuncs = map calculateOffsetInnerOuter offsets
                        in offsetFuncs <*> cs --`using` (parList rdeepseq)

-- Slice all contours according to _sliceHeight specified in SliceParams
sliceContours :: [Triangle] -> SliceParams -> [[Either InnerContour OuterContour]]
sliceContours m sp = map (generateContour m) (calcSliceOffsets (meshFloor m) (view (sliceHeight._1) sp) (meshCeil m)) `using` (parList rdeepseq)

calcSliceOffsets :: Double -> Double -> Double -> [Double]
calcSliceOffsets ch sh mh
        | ch >= mh = [mh]
        | otherwise = ch : calcSliceOffsets (ch+sh) sh mh

-- Because our contours are absolute points we can convert them to absolute G-Code moves
toGCmd :: [[Combination]] -> [GCmd]
toGCmd cs = cs ^.. (each. each . folding (\c -> return (AbsProgr [GArg {name= "X", value = Just $ showFFloat (Just 6) (c ^. (position.xCoord)) "" },
                                                                  GArg {name= "Y", value = Just $ showFFloat (Just 6) (c ^. (position.yCoord)) ""},
                                                                  GArg {name= "Z", value = Just $ showFFloat (Just 6) (c ^. (position.zCoord)) ""},
                                                                  GArg {name= "E", value = Just $ showFFloat (Just 6) (c ^. (physics.extMove._1)) ""}]
                                                       ) :: [GCmd]))

-- Offsets a contour list with itself by 1 to find out the distance for each move between points
-- then calculates corresponding print parameters
printPrep :: [[Vertex]] -> SliceParams -> [[Combination]]
printPrep cs sp = let neighbours = map (\c -> (head c, head c) : zip c (tail c)) cs
                  in over each (\ x -> x ^.. (each . folding (\(v1, v2) -> return (Comb v2 (calcPParams v1 v2 sp)) :: [Combination]))) neighbours

-- Calculates printing parameters for each move
calcPParams :: Vertex -> Vertex -> SliceParams -> PrintParams
calcPParams v1 v2 sp = PParams {_extMove = calcMotorDistance (calcExtrVol v1 v2 sp) (sp ^. filamentWidth),
                                _velocity   = sp ^. speed}

-- Assumes that the laid-down material has a rectangular cross-section
-- the rectangle width being the nozzleWidth and height being the sliceHeight
calcExtrVol :: Vertex -> Vertex -> SliceParams -> Unit
calcExtrVol v1 v2 sp = ((vertexDistance v1 v2) * (sp ^. nozzleAttributes.nozzleWidth._1) * (sp ^. sliceHeight._1), "mm3")

-- Calculate actual extruder motor movement
-- using Extrusion Volume and Filament Diameter
calcMotorDistance :: Unit -> Unit -> Unit
calcMotorDistance ev fd = (ev^._1 / circleArea (fd^._1 / 2) , "mm")

circleArea :: Double -> Double
circleArea r = pi * r ^ 2

-- TODO: Calculate Extrusion Amount per Combination and adjust speed to not exceed maxExtrusionAmount
maxExtrusionBarrier :: [[Combination]] -> [[Combination]]
maxExtrusionBarrier = undefined