definition module Config

from Data.Error import :: MaybeError
from System.FilePath import :: FilePath
from Text.YAML import :: YAMLErrorWithLocations
from Text.YAML.Compose import :: YAMLNode
from Text.YAML.Construct import generic gConstructFromYAML
from Text.YAML.Schemas import :: YAMLSchema, failsafeSchema, jsonSchema, coreSchema

//* Shadow for `CompilerSettings` on which `gConstructFromYAML` can be derived.
:: CompilerSettingsConfig =
	{ compiler :: !FilePath
		//* Compiler's executable name (e.g. `cocl`, `cocl-itasks`), supposed
		//* to be found in `CLEAN_HOME/EXE_PATH`.
	, libraries :: ![String]
		//* Library names, found in `CLEAN_HOME/LIB_PATH`.
	, paths :: ![FilePath]
		//* Extra search paths, either absolute or relative to
		//* `PROJECT_FILENAME`.
	}

derive gConstructFromYAML CompilerSettingsConfig

fetchConfig :: ![!FilePath] !*World -> (!MaybeError String CompilerSettingsConfig, !*World)

:: EastwoodState = {workspaceFolders :: ![!FilePath]}
