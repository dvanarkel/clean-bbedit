implementation module Compiler

import StdEnv
import Data.Error, Data.Maybe, Data.Func, Data.Functor, Data.List
import Text
//import Text.YAML
import System.Environment, System.File, System.FilePath, System.Process
from Eastwood.Diagnostic import
	:: EastwoodDiagnostic {..}, :: DiagnosticSource, :: EastwoodDiagnosticSeverity, :: CharacterRange, :: EastwoodRange,
	:: EastwoodPosition
import qualified Eastwood.Diagnostic
import Eastwood.Range

:: SearchPaths :== [FilePath]

CLEAN_HOME_ENV_VAR :== "CLEAN_HOME"
COCL_RELATIVE_PATH :== "/lib/exe/cocl"
WARNING :== "warning"

// TODO: Lifterror or something
// TODO: Monad?

runCompiler :: !FilePath !*World -> (!MaybeError String [EastwoodDiagnostic], !*World)
runCompiler fp world
	//# (mbConfig, world) = readFile "Eastwood.yml" world
	// Check if we could parse the yml file
	//| isError mbConfig = (Error o toString $ fromError mbConfig, world)
	//# config = fromOk config
	// Parse the YAML, ignore warnings
	//# mbYML = loadYAML coreSchema config
	//| isError mbYAML = (Error o toString $ fromError mbYAML, world)
	# searchPaths = []//fromOk mbYML
	# (mbOutput, world) = callCocl fp searchPaths world
	= (Ok $ diagnosticsFor (takeFileName fp) $ fromOk mbOutput, world)

callCocl :: !FilePath ![FilePath] !*World -> (!MaybeError String String, !*World)
callCocl fp searchPaths world
	// Get CLEAN_HOME
	# (mbCleanHome, world) = getEnvironmentVariable CLEAN_HOME_ENV_VAR world
	| isNone mbCleanHome = (Error (concat3 "Could not get " CLEAN_HOME_ENV_VAR " environment variable"), world)
	# cleanHome = fromJust mbCleanHome
	# coclPath = cleanHome +++ COCL_RELATIVE_PATH
	# searchPaths = concatPaths [takeDirectory fp: searchPaths]
	// TODO: Don't write file
	# (mbHandle, world) = runProcessIO coclPath ["-P", searchPaths, dropExtension $ takeFileName fp] ?None world
	| isError mbHandle = (Error o snd $ fromError mbHandle, world)
	# (handle, io) = fromOk mbHandle
	# (mbOsError, world) = waitForProcess handle world
	| isError mbOsError = (Error o snd $ fromError mbOsError, world)
	# (mbOutput, world) = readPipeBlocking io.stdErr world
	| isError mbOutput = (Error o snd $ fromError mbOutput, world)
	# (mbOsError, world) = closeProcessIO io world
	| isError mbOsError = (Error o snd $ fromError mbOsError, world)
	= (Ok $ fromOk mbOutput, world)
where
	concatPaths :: ![FilePath] -> String
	concatPaths paths = join ":" paths

:: DiagnosticSource | Compiler

diagnosticsFor :: !String !String -> [EastwoodDiagnostic]
diagnosticsFor fileName output = diagnosticsForAccum 0 ?None []
where
	diagnosticsForAccum :: !Int !(?(Int, Int)) ![EastwoodDiagnostic] -> [EastwoodDiagnostic]
	diagnosticsForAccum idx previousStart acc
		| locationBlockIdx == -1 = acc`
		| otherwise = diagnosticsForAccum (inc locationBlockIdx) (?Just (lineNr, newlineBeforeLocBlock + 1)) acc`
	where
		acc` =
			case previousStart of
				?Just (lineNr, startIdx) = [diagnosticFor lineNr $ output % (startIdx, previousStartIdx): acc]
				?None                  = acc

		lineNr                = toInt $ (output % (lineNrStartIdx, lastDigitAfter lineNrStartIdx))
		lineNrStartIdx        = locationBlockIdx + size locationBlockStart
		newlineBeforeLocBlock = indexOfNewlineBefore locationBlockIdx
		previousStartIdx      = if (locationBlockIdx == -1) (size output - 1) (newlineBeforeLocBlock - 1)
		locationBlockIdx      = indexOfAfter idx locationBlockStart output
		locationBlockStart    = concat3 "[" fileName ","

		indexOfNewlineBefore :: !Int -> Int
		indexOfNewlineBefore -1  = -1
		indexOfNewlineBefore idx = if (output.[idx] == '\n') idx (indexOfNewlineBefore $ dec idx)

		lastDigitAfter :: !Int -> Int
		lastDigitAfter idx = if (isDigit output.[idx]) (lastDigitAfter $ inc idx) (idx - 1)

diagnosticFor :: !Int !String -> EastwoodDiagnostic
diagnosticFor lineNr line =
	{ range =
		// Line number of the Clean compiler are 1-based, but we need 0-based line numbers.
		{start = {line = lineNr - 1, character = 0}, end = {line = lineNr, character = 0}}
	, severity = if (indexOf WARNING line == -1) 'Eastwood.Diagnostic'.Error 'Eastwood.Diagnostic'.Warning
	, dCode    = 0
	, source   = Compiler
	, message  = line
	}
