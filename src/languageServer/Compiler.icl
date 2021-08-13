implementation module Compiler

import StdEnv
import Data.Error
import Data.Maybe
import Data.Func
import Data.Functor
import Data.List
import qualified Data.Map
from Data.Map import :: Map
import System.OS
import System.Process
import Text
from Eastwood.Diagnostic import
	:: Diagnostic {..}, :: DiagnosticSource, :: DiagnosticSeverity, :: CharacterRange,
	:: Position
import qualified Eastwood.Diagnostic
import Eastwood.Range

WARNING_LOWER :== "warning"
WARNING_UPPER :== "Warning"

runCompiler :: !FilePath !String !CompilerSettings !*World -> (!MaybeError String (Map FilePath [!Diagnostic]), !*World)
runCompiler moduleFile moduleName config world
	# (mbCoclResult, world) = callCocl moduleFile moduleName config world
	| isError mbCoclResult = (liftError mbCoclResult, world)
	# (retCode, output) = fromOk mbCoclResult
	# diagnostics = diagnosticsFor (moduleName <.> takeExtension moduleFile) output
	// If the return code is not 0, either problems have been detected in the file or the file could not be processed.
	// In the later case (no diagnostics could be extracted from the output)
	// we generate an error instead of diagnostics.
	| retCode <> 0 && noDiagnostics diagnostics = (Error ("The compiler crashed with the output:\n" +++ output), world)
	= (Ok diagnostics , world)
where
	// Because diagnosticsFor will always return an entry for the requested
	// module, we cannot use `'Data.Map'.null`. The diagnostics are empty when
	// there is one entry, and this entry has no diagnostics.
	noDiagnostics diagnostics = ('Data.Map'.toList diagnostics) =: [(_, [|])]

/**
 * Executes the cocl on the given files (which can be a ICL or DCL) and results in the resulting output.
 * @param The file cocl has to be called on
 * @param The module name
 * @param The settings for the compiler, contains mostly the search path
 * @result Either an error or cocl's output
 */
callCocl :: !FilePath !String !CompilerSettings !*World -> (!MaybeError String (Int, String), !*World)
callCocl moduleFile moduleName {compilerPath, searchPaths} world
	// Call cocl
	# (mbHandle, world) =
		runProcessIO compilerPath ["-c", "-P", concatPaths searchPaths, moduleName] ?None world
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

:: DiagnosticSource | Compiler

/**
 * diagnosticsFor moduleFile output = diagnostics:
 *     `diagnostics` are the diagnostics in the compiler output `output` for `moduleFile`.
 */
diagnosticsFor :: !FilePath !String -> Map FilePath [!Diagnostic]
diagnosticsFor moduleFile output =
	// The compiler has filenames with . instead of / (e.g. Data.Error.icl); we
	// need to convert these to real filenames.
	'Data.Map'.foldrWithKey ('Data.Map'.put o fixFileName) 'Data.Map'.newMap $
	diagnosticsForAccum 0 ?None $
	// We always have to generate a result for `moduleFile`.
	// If there are no diagnostics we have to report that to the client to clear possibly present diagnostics.
	'Data.Map'.singleton moduleFile [!]
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

	// replace all .s except the one of the extension with /s
	fixFileName s =
		{ if (c == '.' && i <> size s - 4) OS_PATH_SEPARATOR c
		\\ c <-: s & i <- [0..]
		}

diagnosticFor :: !Int !String -> Diagnostic
diagnosticFor lineNr line =
	{ range =
		// Line number of the Clean compiler are 1-based, but we need 0-based line numbers.
		{start = {line = lineNr - 1, character = 0}, end = {line = lineNr - 1, character = 999999}}
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
