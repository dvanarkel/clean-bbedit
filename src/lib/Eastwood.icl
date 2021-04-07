implementation module Eastwood

import StdEnv

import Clean.Parse
import Data.Error
from Data.Func import $
from Data.Functor import class Functor(..), <$>
import Data.Maybe
from System.File import :: FileError, instance toString FileError, readFile
from System.FilePath import :: FilePath
from Text import class Text(split), instance Text String

import Eastwood.Configuration
from Eastwood.Diagnostic import :: Diagnostic(..), :: DiagnosticSource, :: DiagnosticSeverity
import qualified Eastwood.Pass.TrailingWhitespace

runPassesFile :: !Configuration !FilePath !*World -> (MaybeError String [Diagnostic], !*World)
runPassesFile configuration filePath world
	#! (mbContents, world) = readFile filePath world
	| isError mbContents = (Error (toString (fromError mbContents)), world)
	#! (mbParsedModule, world) = readModule filePath world
	| isError mbParsedModule
		= (liftError mbParsedModule, world)
		= (Ok $ runPasses configuration (splitLines (fromOk mbContents)) (error2mb mbParsedModule), world)

runPassesString :: !Configuration !String -> [Diagnostic]
runPassesString configuration contents = runPasses configuration (splitLines contents) ?None

/**
 * @param The configuration.
 * @param The lines of the file.
 * @param Optionally, the parsed syntax tree. When not given, some passes in the configuration may be ignored.
 */
runPasses :: !Configuration ![String] !(?(ParsedModule, HashTable)) -> [Diagnostic]
runPasses {lineRanges, passes} contents mbParsedModule
	# diagnostics = map runPasses` passes
	= flatten $ map (filterDiagnostics lineRanges) diagnostics
where
	runPasses` :: !PassConfiguration -> [Diagnostic]
	runPasses` (TrailingWhitespaceConfiguration passConfig) =
		withLines 'Eastwood.Pass.TrailingWhitespace'.runPass passConfig

	// helper functions for runPasses`
	withLines run config = run config contents

	filterDiagnostics :: ![LineRange] ![Diagnostic] -> [Diagnostic]
	filterDiagnostics _ [] = []
	filterDiagnostics [] _ = []
	filterDiagnostics lrx=:[lr:lrs] dsx=:[d=:{range}:ds]
		| inLineRange lr range = [d:filterDiagnostics lrx ds]
		| afterLineRange lr range = filterDiagnostics lrs dsx
		| otherwise = filterDiagnostics lrx ds

splitLines :: !String -> [String]
splitLines string = splitLines` string 0
where
	splitLines` :: !String !Int -> [String]
	splitLines` string i = case findNewLine string i of
		?None = if (size string == i) [] [string % (i, size string - 1)]
		?Just (start, end) = [string % (i, start - 1) : splitLines` string (end + 1)]
	where
		// Results in the index of the first newline character and the index of the last newline character
		// For \n these will always be the same, for \r\n these will be different by 1
		findNewLine :: !String !Int -> ?(Int, Int)
		findNewLine str i
			| i >= size str = ?None
			| str.[i] == '\n' = ?Just (i, i)
			| i >= (size str) - 1 = ?None
			| str.[i] == '\r' && str.[i + 1] == '\n' = ?Just (i, i + 1)
			| otherwise = findNewLine str (i + 1)
