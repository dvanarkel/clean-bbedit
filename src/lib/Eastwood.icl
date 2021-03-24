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
runPassesString configuration contents = runPassesLines configuration (split "\n" contents)

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
