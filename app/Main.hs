module Main where
import Lib3mf
import LibHslicer.PlanarSlice
import LibGcode
import System.Environment
import Control.Lens
import Options.Applicative as OA
import Data.Semigroup ((<>))

-- Mirror of sliceParams data structure + filepaths
data Args = Args
    { sliceHeight :: Double
    , contourLines :: Int
    , maxVelocity :: Double
    , filamentWidth :: Double
    , nozzleWidth :: Double
    , maxExtrusionAmount :: Double
    , outputfp :: String
    , inputfp :: String
    }

-- Parser for handling command line arguments
args :: Parser Args 
args = Args
        <$> option auto
            ( long "sliceHeight"
            <> short 's'
            <> help "The distance (in mm) between each slice"
            <> showDefault 
            <> OA.value 0.2
            <> metavar "DOUBLE")
        <*> option auto
            ( long "contourLines"
            <> short 'c'
            <> help "The amount of offsets per contour"
            <> showDefault 
            <> OA.value 4
            <> metavar "INT")
        <*> option auto
            ( long "maxVelocity"
            <> short 'v'
            <> help "The maximum velocity (in mm/s) the printer moves at"
            <> showDefault
            <> OA.value 20
            <> metavar "DOUBLE")
        <*> option auto
            ( long "filamentWidth"
            <> short 'f'
            <> help "The width of the filament (in mm) that will be used for printing the object"
            <> showDefault
            <> OA.value 1.75
            <> metavar "DOUBLE")
        <*> option auto 
            ( long "nozzleWidth"
            <> short 'n'
            <> help "The nozzle width (in mm) of the 3D printer which will print your object"
            <> showDefault
            <> OA.value 0.4
            <> metavar "DOUBLE")
        <*> option auto 
            ( long "maxExtrusionAmount"
            <> help "The maximum amount of filament (in mm3/s) that can be extruded by the 3D printer which will print your object"
            <> showDefault
            <> OA.value 0.2
            <> metavar "DOUBLE")
        <*> strOption
            ( long "outputfp"
            <> short 'o'
            <> metavar "FILEPATH"
            <> help "The filepath where the gcode file should be saved" )
        <*> OA.argument str (metavar "FILEPATH")

main :: IO ()
main = doslice =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> progDesc "Slice a 3mf file into gcode"
     <> header "HSlicer" )

-- Actually slicing the mesh
doslice :: Args -> IO ()
doslice (Args sh cl v fw nw mea outputfp inputfp) = do
    let nozzleAttrib = NAttrib { _nozzleWidth = (nw,"mm"), _maxExtrusionAmount = (mea,"mm3/s")}
        sliceParams = SParams { _sliceHeight = (sh,"mm"), _contourLines = cl, _speed = (v,"mm/s"), _filamentWidth = (fw,"mm"), _nozzleAttributes = nozzleAttrib }
    vertices <- parseVertices inputfp
    triangles <- parseTriangles inputfp vertices
    writeGCode outputfp (sliceMesh triangles sliceParams)