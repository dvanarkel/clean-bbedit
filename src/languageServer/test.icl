module test

import StdEnv
import System.FilePath
import Data.Error
import Text.YAML
import Text
import System.File
import System.Directory
import Text.YAML.Construct

:: TestRecord = {x :: ?Int, y :: ?Int, z :: Int}

:: ADT = A String | B Real | C

derive gConstructFromYAML TestRecord
derive gConstructFromYAML ADT

Start :: !*World -> MaybeError YAMLErrorWithLocations (TestRecord, [String])
Start w
	# (Ok curDir, w) = getCurrentDirectory w
	# (Ok fileContents, w) = readFile (curDir </> "test.yml") w
	= loadYAML coreSchema fileContents

