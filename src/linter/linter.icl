module linter

from ArgEnv import getCommandLine
import StdEnv

from Control.Monad import class <*>, class Applicative, class pure
from Control.Monad import >>=, class Monad(bind)
import qualified Data.Error
from Data.Func import $
from Data.Functor import class Functor
from Data.List import intersperse
from System.File import :: FileError, instance toString FileError
from System.FilePath import :: FilePath
import System.IO
from Text import concat3, class Text(concat), instance Text String

import Eastwood
import Eastwood.Configuration
import Eastwood.Diagnostic

instance toString Diagnostic
where
	toString d = concat
		[ colorCode d.Diagnostic.severity
		, toString d.Diagnostic.severity
		, ": "
		, d.source
		, "."
		, toString d.dCode
		, ": "
		, toString d.range
		, ": "
		, toString d.message
		, clearColorCode
		]
	where
		colorCode :: !DiagnosticSeverity -> String
		colorCode Error = "\x1b[31m" // Red
		colorCode Warning = "\x1b[33m" // Yellow
		colorCode Information = "\x1b[34m" // Blue
		colorCode Hint = "\x1b[36m" // Cyan

		clearColorCode :: String
		clearColorCode = "\x1b[0m" // Reset all attributes

instance toString DiagnosticSeverity
where
	toString Error = "Error"
	toString Warning = "Warning"
	toString Information = "Info"
	toString Hint = "Hint"

instance toString (Range t) | toString t
where
	toString { Range | start, end } = concat3 (toString start) "-" (toString end)

instance toString Position
where
	toString { Position | line, character } = concat3 (toString line) ":" (toString character)


Start :: !*World -> *World
Start world = execIO startIO world

startIO :: IO ()
startIO
	#! args = getCommandLine
	#! filePath = args.[1]
	= withWorld (runPassesFile defaultConfiguration filePath) >>= showResult

showResult :: ('Data.Error'.MaybeError FileError [Diagnostic]) -> IO ()
showResult ('Data.Error'.Error fileError) = putStrLn $ toString fileError
showResult ('Data.Error'.Ok diagnostics) = putStrLn o concat o intersperse "\n" $ map toString diagnostics

defaultConfiguration :: Configuration
defaultConfiguration =
	{ Configuration
	| lineRanges = [{ Range | start = ?None, end = ?None }]
	, passes = [ TrailingWhitespaceConfiguration defaultTrailingWhitespaceConfiguration ]
	}
where
	defaultTrailingWhitespaceConfiguration :: TrailingWhitespaceConfiguration
	defaultTrailingWhitespaceConfiguration =
		{ TrailingWhitespaceConfiguration
		| severity = ?None
		}
