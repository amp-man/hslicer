{-# LANGUAGE TemplateHaskell #-}

module LibHslicer.PlanarSlice where

import TriangleMesh ( Vertex, Triangle, meshFloor, meshCeil )
import LibGcode
import LibHslicer.Contour
import Control.Lens

-- Düsengröße, Slice-Höhe,...
data SliceParams = SParams { _sliceHeight, _nozzleWidth::Double } deriving Show
sParamsDefault = SParams { _sliceHeight = 0.2, _nozzleWidth = 0.4 }

-- Pro Pfadsegment: Extrusionsvolumen, Geschwindigkeit,...
data PrintParams = PParams {_extVolume, _speed :: Double} deriving Show

data Combination = Comb {_position :: Vertex, _physics :: PrintParams} deriving Show

makeLenses ''SliceParams
makeLenses ''PrintParams
makeLenses ''Combination

sliceMesh :: [Triangle] -> SliceParams -> [GCmd]
sliceMesh m sp = undefined 

sliceContours :: [Triangle] -> SliceParams -> [[Either InnerContour OuterContour]]
sliceContours m sp = map (generateContour m) (calcSliceOffsets (meshFloor m) (view sliceHeight sp) (meshCeil m))

calcSliceOffsets :: Double -> Double -> Double -> [Double]
calcSliceOffsets ch sh mh
        | ch >= mh = [mh]
        | otherwise = ch : calcSliceOffsets (ch+sh) sh mh


-- sParamsTest = SParams {_sliceHeight=0.5, _nozzleWidth=0.6}

-- params = set sliceHeight 0.2 pParamDefault

-- combi = Comb {_position=Vertex 0.0 0.0 0.0, _physics=PParams { _extVolume = 1, _speed = 10}}

-- importParams :: Combination -> Combination
-- importParams = over (physics.sliceHeight) (+1)