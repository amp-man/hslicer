{-# LANGUAGE TemplateHaskell #-}

module LibHslicer.PlanarSlice where

import TriangleMesh
import LibGcode
import LibHslicer.Contour
import Control.Lens

type Unit = (Double, String)

-- Düsengröße, Slice-Höhe,...
data NozzleAttrib = NAttrib { _nozzleWidth, _maxExtrusionAmount::Unit} deriving Show
data SliceParams = SParams { _sliceHeight::Unit, _speed::Unit, _filamentWidth::Unit, _nozzleAttributes::NozzleAttrib } deriving Show
nAttribDefault = NAttrib { _nozzleWidth = (0.4,"mm"), _maxExtrusionAmount = (0.2,"mm3/s")}
sParamsDefault = SParams { _sliceHeight = (0.2,"mm"), _speed = (20,"mm/s"), _filamentWidth = (1.75,"mm"), _nozzleAttributes = nAttribDefault }

-- Pro Pfadsegment: Extrusionsvolumen, Geschwindigkeit,...
data PrintParams = PParams {_extMove, _velocity :: Unit} deriving (Eq, Show)

data Combination = Comb {_position :: Vertex, _physics :: PrintParams} deriving Show

makeLenses ''SliceParams
makeLenses ''NozzleAttrib
makeLenses ''PrintParams
makeLenses ''Combination

-- Calculate Extrusion Amount per Combination and adjust speed to not exceed maxExtrusionAmount
maxExtrusionBarrier :: [[Combination]] -> [[Combination]]
maxExtrusionBarrier = undefined

-- sliceMesh :: [Triangle] -> SliceParams -> [GCmd]
-- sliceMesh m sp = toGCmd $ printPrep (map (calculateOffsetInnerOuter (-0.2)) (sliceContours m sp)) sp

toGCmd :: [[Combination]] -> [GCmd]
toGCmd cs = cs ^.. (each. each . folding (\c -> return (AbsProgr [GArg {name= "X", value = Just $ show $ c ^. (position.xCoord)}, 
                                                                  GArg {name= "Y", value = Just $ show $ c ^. (position.yCoord)}, 
                                                                  GArg {name= "Z", value = Just $ show $ c ^. (position.zCoord)}, 
                                                                  GArg {name= "E", value = Just $ show $ c ^. (physics.extMove._1)}]
                                                       ) :: [GCmd]))

printPrep :: [[Vertex]] -> SliceParams -> [[Combination]]
-- printPrep cs sp = map (\ x -> x ^.. (each . folding (\v -> return (Comb v (calcExtrVol v sp)) :: [Combination]))) cs
printPrep cs sp = let neighbours = map (\c -> (head c, head c) : zip c (tail c)) cs
                      --pparams = PParams{_extmove = calcMotorDistance (calcExtrVol v sp) (sp^.filamentWidth._1), _speed= sp ^. speed._1}
                  in over each (\ x -> x ^.. (each . folding (\(v1, v2) -> return (Comb v2 (calcPParams v1 v2 sp)) :: [Combination]))) neighbours
--printPrep cs sp = over (traverse . each) (\v -> return (Comb v (calcExtrVol v sp))) cs

calcPParams :: Vertex -> Vertex -> SliceParams -> PrintParams
calcPParams v1 v2 sp = PParams {_extMove = calcMotorDistance (calcExtrVol v1 v2 sp) (sp ^. filamentWidth),
                                _velocity   = sp ^. speed}

calcExtrVol :: Vertex -> Vertex -> SliceParams -> Unit
calcExtrVol v1 v2 sp = ((vertexDistance v1 v2) * (sp ^. nozzleAttributes.nozzleWidth._1) * (sp ^. sliceHeight._1), "mm3")

calcMotorDistance :: Unit -> Unit -> Unit
calcMotorDistance (ev, evu) (fw, fwu) =
  let fa = pi * (fw / 2) ^ 2
   in (ev / fa, "mm2")

sliceContours :: [Triangle] -> SliceParams -> [[Either InnerContour OuterContour]]
sliceContours m sp = map (generateContour m) (calcSliceOffsets (meshFloor m) (view (sliceHeight._1) sp) (meshCeil m))

calcSliceOffsets :: Double -> Double -> Double -> [Double]
calcSliceOffsets ch sh mh
        | ch >= mh = [mh]
        | otherwise = ch : calcSliceOffsets (ch+sh) sh mh

-- sParamsTest = SParams {_sliceHeight=0.5, _nozzleWidth=0.6}

-- params = set sliceHeight 0.2 pParamDefault

-- combi = Comb {_position=Vertex 0.0 0.0 0.0, _physics=PParams { _extVolume = 1, _speed = 10}}

-- importParams :: Combination -> Combination
-- importParams = over (physics.sliceHeight) (+1)