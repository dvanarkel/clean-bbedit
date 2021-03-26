implementation module Eastwood

import StdEnv

from Data.Error import :: MaybeError(..)
from Data.Func import $
from System.File import :: FileError, readFile
from System.FilePath import :: FilePath
from Text import class Text(split), instance Text String

import Eastwood.Configuration
from Eastwood.Diagnostic import :: Diagnostic(..)
import qualified Eastwood.Pass.TrailingWhitespace

runPassesFile :: !Configuration !FilePath !*World -> (MaybeError FileError [Diagnostic], !*World)
runPassesFile configuration filePath world
	#! (mberror, world) = readFile filePath world
	= case mberror of
		Error s -> (Error s, world)
		Ok contents -> (Ok $ runPassesString configuration contents, world)

runPassesString :: !Configuration !String -> [Diagnostic]
runPassesString configuration contents = runPassesLines configuration (splitLines contents)

// Function is not exported because future passes might not work for this format. Unintended behavior might occur when
// assuming the strings can be concatenated. This is thus best left to the user of the library.
runPassesLines :: !Configuration ![String] -> [Diagnostic]
runPassesLines {lineRanges, passes} contents
	# diagnostics = map runPassesLines` passes
	= flatten $ map (filterDiagnostics lineRanges) diagnostics
where
	runPassesLines` :: !PassConfiguration -> [Diagnostic]
	runPassesLines` (TrailingWhitespaceConfiguration passConfig) =
		'Eastwood.Pass.TrailingWhitespace'.runPass passConfig contents

	filterDiagnostics :: ![LineRange] ![Diagnostic] -> [Diagnostic]
	filterDiagnostics _ [] = []
	filterDiagnostics [] _ = []
	filterDiagnostics lrx=:[lr:lrs] dsx=:[d=:{range}:ds]
		| inLineRange lr range = [d:filterDiagnostics lrx ds]
		| afterLineRange lr range = filterDiagnostics lrs dsx
		| otherwise = filterDiagnostics lrx dsx

splitLines :: !String -> [String]
splitLines string = splitLines` string 0
where
	splitLines` :: !String !Int -> [String]
	splitLines` string i = case findNewLine string i of
		?None = []
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
