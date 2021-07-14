module eastwoodTests

import StdEnv
import StdMisc

from Data.Error import fromError, fromOk, isError
import System.Directory
import System.File
from System.FilePath import </>
import System._Unsafe

import Gast
import Gast.CommandLine

import Eastwood
import Eastwood.Configuration
import Eastwood.Diagnostic
from Eastwood.Pass.BasicValueCAFs import :: BasicValueCAFsConfiguration{..}, BasicValueCAFsPass
from Eastwood.Pass.TrailingWhitespace import :: TrailingWhitespaceConfiguration{..}, TrailingWhitespacePass
import Eastwood.Range

TMP_PATH :== "/tmp/eastwood-test"

derive gEq Diagnostic, DiagnosticSeverity, Position, Range
derive genShow Diagnostic, DiagnosticSeverity, Position, Range
derive gPrint Diagnostic, DiagnosticSeverity, Position, Range

instance == Diagnostic
where
	(==) d1 d2 = d1 === d2

gEq{|DiagnosticSource|} BasicValueCAFsPass x = x=:BasicValueCAFsPass
gEq{|DiagnosticSource|} TrailingWhitespacePass x = x=:TrailingWhitespacePass
genShow{|DiagnosticSource|} _ _ BasicValueCAFsPass rest = ["BasicValueCAFsPass": rest]
genShow{|DiagnosticSource|} _ _ TrailingWhitespacePass rest = ["TrailingWhitespacePass": rest]
gPrint{|DiagnosticSource|} BasicValueCAFsPass st = gPrint{|*|} "BasicValueCAFsPass" st
gPrint{|DiagnosticSource|} TrailingWhitespacePass st = gPrint{|*|} "TrailingWhitespacePass" st

defaultConfiguration :: Configuration
defaultConfiguration =
	{ Configuration
	| lineRanges = [{ Range | start = ?None, end = ?None }]
	, passes =
		[ BasicValueCAFsConfiguration defaultBasicValueCAFsConfiguration
		, TrailingWhitespaceConfiguration defaultTrailingWhitespaceConfiguration
		]
	}
where
	defaultBasicValueCAFsConfiguration :: BasicValueCAFsConfiguration
	defaultBasicValueCAFsConfiguration =
		{ BasicValueCAFsConfiguration
		| severity = ?None
		}

	defaultTrailingWhitespaceConfiguration :: TrailingWhitespaceConfiguration
	defaultTrailingWhitespaceConfiguration =
		{ TrailingWhitespaceConfiguration
		| severity = ?None
		}

Start :: *World -> *World
Start world = exposeProperties [OutputTestEvents] [Bent] properties world

properties :: [Property]
properties =:
	[ helloWorld as "hello world"
	, helloWorldTrailingWhitespace as "hello world trailing whitespace"
	, basicValueCAFs as "CAFs with and without basic values"
	]

diagnostics :: !Configuration !String !String -> [Diagnostic]
diagnostics configuration moduleName moduleContents = accUnsafe run
where
	run w
		# (mbErr,w) = ensureDirectoryExists TMP_PATH w
		| isError mbErr = abort ("Error while creating temporary directory: " +++ snd (fromError mbErr) +++ "\n")
		# (mbErr,w) = writeFile file moduleContents w
		| isError mbErr = abort ("Error while creating temporary module: " +++ toString (fromError mbErr) +++ "\n")
		# (mbDiagnostics,w) = runPassesFile configuration file w
		| isError mbErr = abort ("Error while running linter: " +++ fromError mbDiagnostics +++ "\n")
		| otherwise = (fromOk mbDiagnostics, w)
	where
		file = TMP_PATH </> moduleName +++ ".icl"

helloWorld :: Property
helloWorld = output =.= expectedOutput
where
	output = diagnostics defaultConfiguration "test" input
	input = "module test\nStart = \"Hello, World\"\n"
	expectedOutput = []

helloWorldTrailingWhitespace :: Property
helloWorldTrailingWhitespace = output =.= expectedOutput
where
	output = diagnostics defaultConfiguration "test" input
	input = "module test\t\n \nStart = \"Hello, World\"\n"
	expectedOutput =
		[
			{ Diagnostic
			| range =
				{ Range
				| start = { Position | line = 1, character = 11 }
				, end = { Position | line = 1, character = 12 }
				}
			, severity = Warning
			, dCode = 0
			, source = TrailingWhitespacePass
			, message = "Found trailing whitespace"
			}
		,
			{ Diagnostic
			| range =
				{ Range
				| start = { Position | line = 2, character = 0 }
				, end = { Position | line = 2, character = 1 }
				}
			, severity = Warning
			, dCode = 0
			, source = TrailingWhitespacePass
			, message = "Found trailing whitespace"
			}
		]

basicValueCAFs :: Property
basicValueCAFs =
	run "x = 5" =.= [] /\ /* normal function with basic value */
	run "x =: [5]" =.= [] /\ /* CAF with non-basic value */
	run "x =: 5" =.= /* CAF with basic value */
		[
			{ Diagnostic
			| range =
				{ Range
				| start = { Position | line = 2, character = 1 }
				, end = { Position | line = 2, character = 6 }
				}
			, severity = Warning
			, dCode = 0
			, source = BasicValueCAFsPass
			, message = "CAF 'x' with a basic value '5' would be faster as a normal function or macro"
			}
		]
where
	run def = diagnostics defaultConfiguration "test" ("module test\n" +++ def)
