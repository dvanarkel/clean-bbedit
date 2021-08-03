implementation module Compiler

import StdEnv
import Data.Error
import Data.Maybe
import Data.Func
import Data.Functor
import Data.List
import qualified Data.Map
from Data.Map import :: Map
import Text
import Text.YAML
import System.Environment, System.File, System.FilePath, System.Process, System.Directory
from Eastwood.Diagnostic import
	:: Diagnostic {..}, :: DiagnosticSource, :: DiagnosticSeverity, :: CharacterRange,
	:: Position
import qualified Eastwood.Diagnostic
import Eastwood.Range

CLEAN_HOME_ENV_VAR :== "CLEAN_HOME"
EXE_PATH :== "lib/exe"
LIBS_PATH :== "lib"
WARNING_LOWER :== "warning"
WARNING_UPPER :== "Warning"
PROJECT_FILENAME :== "Eastwood.yml"

//* This represents the compiler settings which are provided by the project file.
:: CompilerSettings =
	{ compiler :: !FilePath
		//* compiler's executable name (e.g. `cocl`, `cocl-itasks`), supposed to be found in `CLEAN_HOME/EXE_PATH`.
	, libraries :: ![String] //* libraries of which modules are included (located in `CLEAN_HOME/LIB_PATH`)
	, paths     :: ![FilePath] //* additional paths to search for modules
	}

derive gConstructFromYAML CompilerSettings

runCompiler :: !FilePath ![!FilePath] !*World -> (!MaybeError String (Map FilePath [!Diagnostic]), !*World)
runCompiler moduleFile workspaceFolders world
	# (mbConfigPath, world) = findFile PROJECT_FILENAME workspaceFolders world
	| isNone mbConfigPath = (Error ("Could not find " +++ PROJECT_FILENAME), world)
	# configPath = fromJust mbConfigPath
	# (mbConfig, world) = readFile configPath world
	// Check if we could parse the yml file
	| isError mbConfig =
		( Error (concat4 "Cannot get project settings from " configPath ": " (toString $ fromError mbConfig))
		, world
		)
	# config = fromOk mbConfig
	// Parse the YAML, ignore warnings
	# mbYML = loadYAML coreSchema config
	| isError mbYML =
		(Error $ concat4 "Invalid format of project file " configPath ": " (toString $ fromError mbYML), world)
	# config = fst $ fromOk mbYML
	  // Interpret the paths relative to the path of the configuration file
	  config & paths = [takeDirectory configPath </> p \\ p <- config.paths]
	# (mbOutput, world) = callCocl moduleFile config world
	| isError mbOutput = (liftError mbOutput, world)
	# (retCode, output) = fromOk mbOutput
	# diagnostics = diagnosticsFor (takeFileName moduleFile) output
	// If the return code is not 0, either problems have been detected in the file or the file could not be processed.
	// In the later case (no diagnostics could be extracted from the output)
	// we generate an error instead of diagnostics.
	| retCode <> 0 && 'Data.Map'.null diagnostics = (Error output, world)
	= (Ok diagnostics , world)
where
	findFile :: !FilePath ![!FilePath] !*World -> (!?FilePath, !*World)
	findFile file [|] w = (?None, w)
	findFile file [|dir:dirs] w
		# (exi, w) = fileExists path w
		= if exi (?Just path, w) (findFile file dirs w)
	where
		path = dir </> file

/**
 * Executes the cocl on the given files (which can be a ICL or DCL) and results in the resulting output.
 * @param The file cocl has to be called on
 * @param The settings for the compiler, contains mostly the search path
 * @result Either an error or cocl's output
 */
callCocl :: !FilePath !CompilerSettings !*World -> (!MaybeError String (Int, String), !*World)
callCocl moduleFile {compiler, paths, libraries} world
	// Get CLEAN_HOME
	# (mbCleanHome, world) = getEnvironmentVariable CLEAN_HOME_ENV_VAR world
	| isNone mbCleanHome = (Error (concat3 "Could not get " CLEAN_HOME_ENV_VAR " environment variable"), world)
	# cleanHome = fromJust mbCleanHome
	// Find the compiler executable
	# coclPath = cleanHome </> EXE_PATH </> compiler
	// Get the searchpaths from the CompilerSettings
	# searchPaths = concatPaths [takeDirectory moduleFile: (libPathFor cleanHome <$> libraries) ++ paths]
	// Call cocl
	# (mbHandle, world) =
		runProcessIO coclPath ["-c", "-P", searchPaths, dropExtension $ takeFileName moduleFile] ?None world
	| isError mbHandle = (Error o snd $ fromError mbHandle, world)
	# (handle, io) = fromOk mbHandle
	# (retCode, world) = waitForProcess handle world
	| isError retCode = (Error o snd $ fromError retCode, world)
	// Read cocl's output and return code.
	# (mbOutput, world) = readPipeBlocking io.stdErr world
	| isError mbOutput = (Error o snd $ fromError mbOutput, world)
	# (mbOsError, world) = closeProcessIO io world
	| isError mbOsError = (Error o snd $ fromError mbOsError, world)
	= (Ok (fromOk retCode, fromOk mbOutput), world)
where
	concatPaths :: ![FilePath] -> String
	concatPaths paths = join ":" paths

	libPathFor :: !FilePath !FilePath -> FilePath
	libPathFor cleanHome lib = cleanHome </> LIBS_PATH </> lib

:: DiagnosticSource | Compiler

/**
 * diagnosticsFor moduleFile output = diagnostics:
 *     `diagnostics` are the diagnostics in the compiler output `output` for `moduleFile`.
 */
diagnosticsFor :: !FilePath !String -> Map FilePath [!Diagnostic]
diagnosticsFor moduleFile output =
	// We always have to generate a result for `moduleFile`.
	// If there are no diagnostics we have to report that to the client to clear possibly present diagnostics.
	diagnosticsForAccum 0 ?None $ 'Data.Map'.singleton moduleFile [!]
where
	// accumulates diagnostics, we go through the string using `idx` to avoid constructing intermediate strings
	// the previously found file (module.icl or module.dcl), starting index and line number is provided,
	// as a diagnostic is added when the start index of the next message is found
	diagnosticsForAccum :: !Int !(?(String, Int, Int)) !(Map FilePath [!Diagnostic]) -> Map FilePath [!Diagnostic]
	diagnosticsForAccum idx previousStart acc
		| locationBlockIdx == -1 =
			acc` // no next message found
		| otherwise =
			diagnosticsForAccum
				(startIdxOfNextMessageLine locationBlockIdx) (?Just (fileName, lineNr, newlineBeforeLocBlock + 1)) acc`
	where
		acc` = case previousStart of
			?Just (fileName, lineNr, previousStartIdx) =
				'Data.Map'.alter (?Just o maybe [!diag] (\acc -> [!diag: acc])) fileName acc
			where
				diag = diagnosticFor lineNr $ output % (previousStartIdx, startIdx)
			?None =
				acc

		fileName              = output % (locationBlockIdx + 1, lineNrStartIdx - 2)
		lineNr                = toInt (output % (lineNrStartIdx, lastDigitAfter lineNrStartIdx))
		lineNrStartIdx        = indexOfAfter locationBlockIdx "," output + 1
		newlineBeforeLocBlock = indexOfNewlineBefore locationBlockIdx
		// start index of the current message or end of string if no next message is present
		startIdx              = if (locationBlockIdx == -1) (size output - 1) (newlineBeforeLocBlock - 1)
		// start of block "[fileName, lineNumber..."
		locationBlockIdx      = indexOfAfter idx "[" output

		indexOfNewlineBefore :: !Int -> Int
		indexOfNewlineBefore -1  = -1
		indexOfNewlineBefore idx = if (output.[idx] == '\n') idx (indexOfNewlineBefore $ dec idx)

		lastDigitAfter :: !Int -> Int
		lastDigitAfter idx = if (isDigit output.[idx]) (lastDigitAfter $ inc idx) (idx - 1)

		// The index of the next start of the line containing a new message.
		// All further lines of multi-line message are assumed to start with a ' '.
		startIdxOfNextMessageLine :: !Int -> Int
		startIdxOfNextMessageLine idx
			| idx == size output - 1 = idx
			| output.[idx] == '\n' && (idx + 1 == size output || output.[idx + 1] <> ' ') = idx + 1
			| otherwise = startIdxOfNextMessageLine $ inc idx

diagnosticFor :: !Int !String -> Diagnostic
diagnosticFor lineNr line =
	{ range =
		// Line number of the Clean compiler are 1-based, but we need 0-based line numbers.
		{start = {line = lineNr - 1, character = 0}, end = {line = lineNr, character = 0}}
	, severity =
			// we have to check whether 'warning'/'Warning' occurs before the first '[',
			// as it could also occur in identifiers
			let bracketOpenIdx  = indexOf "[" line in
			let warningIdxLower = indexOf WARNING_LOWER line in
			let warningIdxUpper = indexOf WARNING_UPPER line in
			if
				(		(warningIdxLower <> -1 && warningIdxLower < bracketOpenIdx)
					||
						(warningIdxUpper <> -1 && warningIdxUpper < bracketOpenIdx)
				)
				'Eastwood.Diagnostic'.Warning
				'Eastwood.Diagnostic'.Error
	, dCode    = 0
	, source   = Compiler
	, message  = line
	}
