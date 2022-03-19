{-# LANGUAGE TemplateHaskell #-}

module LibHslicer.PlanarSlice where

import TriangleMesh
import LibGcode
import LibHslicer.Contour
import Control.Lens
import Control.Parallel
import Numeric (showFFloat)
import Control.Parallel.Strategies

type Unit = (Double, String)

-- Düsengröße, Slice-Höhe,...
data NozzleAttrib = NAttrib { _nozzleWidth, _maxExtrusionAmount::Unit} deriving Show
data SliceParams = SParams { _sliceHeight::Unit, _speed::Unit, _filamentWidth::Unit, _nozzleAttributes::NozzleAttrib } deriving Show
nAttribDefault = NAttrib { _nozzleWidth = (0.4,"mm"), _maxExtrusionAmount = (0.2,"mm3/s")}
sParamsDefault = SParams { _sliceHeight = (0.2,"mm"), _speed = (20,"mm/s"), _filamentWidth = (1.75,"mm"), _nozzleAttributes = nAttribDefault }

-- Pro Pfadsegment: Extrusionsvolumen, Geschwindigkeit,...
data PrintParams = PParams {_extMove, _velocity :: Unit} deriving (Eq, Show)

data Combination = Comb {_position :: Vertex, _physics :: PrintParams} deriving (Show, Eq)

makeLenses ''SliceParams
makeLenses ''NozzleAttrib
makeLenses ''PrintParams
makeLenses ''Combination


sliceMesh :: [Triangle] -> SliceParams -> [GCmd]
sliceMesh m sp = toGCmd $ printPrep (map (calculateOffsetInnerOuter (-1)) (sliceContours m sp)) sp
--sliceMesh m sp = toGCmd $ printPrep (map (calculateOffsetInnerOuter (0) $ (fmap (pure makeContourCCW))) (sliceContours m sp)) sp

sliceContours :: [Triangle] -> SliceParams -> [[Either InnerContour OuterContour]] 
sliceContours m sp = map (generateContour m) (calcSliceOffsets (meshFloor m) (view (sliceHeight._1) sp) (meshCeil m)) `using` (parList rdeepseq)

calcSliceOffsets :: Double -> Double -> Double -> [Double]
calcSliceOffsets ch sh mh
        | ch >= mh = [mh]
        | otherwise = ch : calcSliceOffsets (ch+sh) sh mh

toGCmd :: [[Combination]] -> [GCmd]
toGCmd cs = cs ^.. (each. each . folding (\c -> return (AbsProgr [GArg {name= "X", value = Just $ showFFloat (Just 6) (c ^. (position.xCoord)) "" }, 
                                                                  GArg {name= "Y", value = Just $ showFFloat (Just 6) (c ^. (position.yCoord)) ""}, 
                                                                  GArg {name= "Z", value = Just $ showFFloat (Just 6) (c ^. (position.zCoord)) ""}, 
                                                                  GArg {name= "E", value = Just $ showFFloat (Just 6) (c ^. (physics.extMove._1)) ""}]
                                                       ) :: [GCmd]))

printPrep :: [[Vertex]] -> SliceParams -> [[Combination]]
printPrep cs sp = let neighbours = map (\c -> (head c, head c) : zip c (tail c)) cs
                  in over each (\ x -> x ^.. (each . folding (\(v1, v2) -> return (Comb v2 (calcPParams v1 v2 sp)) :: [Combination]))) neighbours

-- Hier maxExtrusionBarrier berechnen und ggf. velocity verringern
calcPParams :: Vertex -> Vertex -> SliceParams -> PrintParams
calcPParams v1 v2 sp = PParams {_extMove = calcMotorDistance (calcExtrVol v1 v2 sp) (sp ^. filamentWidth),
                                _velocity   = sp ^. speed}

calcExtrVol :: Vertex -> Vertex -> SliceParams -> Unit
calcExtrVol v1 v2 sp = ((vertexDistance v1 v2) * (sp ^. nozzleAttributes.nozzleWidth._1) * (sp ^. sliceHeight._1), "mm3")

calcMotorDistance :: Unit -> Unit -> Unit
calcMotorDistance (ev, evu) (fw, fwu) =
  let fa = pi * (fw / 2) ^ 2
   in (ev / fa, "mm")

-- Calculate Extrusion Amount per Combination and adjust speed to not exceed maxExtrusionAmount
maxExtrusionBarrier :: [[Combination]] -> [[Combination]]
maxExtrusionBarrier = undefined