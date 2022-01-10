implementation module Config

import StdEnv
import Data.Error
import Data.Func
import Text
import Data.Maybe
import StdTuple
import Text.YAML
import System.FilePath
import System.File
from System.FilePath import :: FilePath
import Eastwood.Util.FileFinder
import Constants
import Data.Either
import Util

derive gConstructFromYAML CompilerSettingsConfig

fetchConfig :: ![!FilePath] !*World -> (!MaybeError String CompilerSettingsConfig, !*World)
fetchConfig workspaceFolders world
	# (mbConfigPath, world) = findSearchPath PROJECT_FILENAME workspaceFolders world
	| isNone mbConfigPath
		= (Error $
			concat
				[ "Could not find the "
				, PROJECT_FILENAME
				, " project configuration file in the workspace folder. Please create the file in the workspace's root folder. The expected format of the "
				, PROJECT_FILENAME
				, " file is described in "
				, README_LINK
				, "."
				]
			, world)
	# configPath = fromJust mbConfigPath </> PROJECT_FILENAME
	# (mbConfig, world) = readFile configPath world
	// Check if the project file could be read.
	| isError mbConfig =
		(Error $
			concat4 "Cannot read project file found at " configPath ": " (toString $ fromError mbConfig)
		, world)
	# config = fromOk mbConfig
	// Parse the YAML, ignore warnings
	# mbYML = loadYAML coreSchema config
	// Check if the YML could be parsed.
	| isError mbYML
		=
		( Error $
			concat
				[ "Invalid format of project file "
				, configPath
				, ": "
				, (toString $ fromError mbYML)
				, ". The expected format of the project file is described in "
				, README_LINK
				]
		, world)
	# config = fst $ fromOk mbYML
	// Interpret the paths relative to the path of the configuration file.
	// The config file path is included by default, so that a developer doesn't have to explicitly add `.` in the config
	// file to include the root directory.
	# config & paths = [takeDirectory configPath : [takeDirectory configPath </> p \\ p <- config.paths]]
	= (Ok config, world)
