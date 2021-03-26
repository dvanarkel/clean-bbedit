module linter

import StdEnv
import StdMaybe

from Control.Applicative import class <*>, class Applicative, class pure
from Control.Monad import >>=, >>|, class Monad(bind), mapM, mapM_
from Data.Error import instance <*> (MaybeError a), instance Functor (MaybeError a), instance Monad (MaybeError a)
from Data.Error import instance pure (MaybeError a)
import qualified Data.Error
from Data.Func import $
from Data.Functor import <$>, class Functor(fmap)
import Data.GenDefault
from Data.List import intersperse
from System.CommandLine import getCommandLine, setReturnCode
from System.File import :: FileError, instance toString FileError
from System.FilePath import :: FilePath
import System.IO
import System.Options
import Text
import Text.GenParse

import Eastwood
import Eastwood.Configuration
import Eastwood.Diagnostic
import Eastwood.Pass.TrailingWhitespace
import Eastwood.Range

:: Options =
	{ color :: !Bool
	, lines :: ![LineRange]
	, file :: !?FilePath
	}
derive gDefault ?, Range

gDefault{|Options|} =
	{ Options
	| color = True
	, lines = defaultConfiguration.lineRanges
	, file = ?None
	}

invalidOptions :: !Options -> Bool
invalidOptions opts = isNone opts.file

usage :: String
usage = " [options] FILE"

optionDesciption :: Option Options
optionDesciption = WithHelp True $ Options
	[ Flag "--no-color"
		(\opts -> 'Data.Error'.Ok {opts & color = False})
		"Do not use ANSI escape codes for colored output"
	, Shorthand "-l" "--lines" $ Option
		"--lines"
		(\ls opts -> (\ls -> {opts & lines = ls}) <$> (mapM parseLineRange $ split "," ls))
		"LINE_RANGES"
		"Line ranges that should be considered (e.g. \"1-2,5-\")"
	, Operand True
		(\fp opts -> ?Just $ 'Data.Error'.Ok {opts & file = ?Just fp})
		"FILE"
		"The file on which the linter should be ran"
	]
where
	parseLineRange :: String -> 'Data.Error'.MaybeError [String] LineRange
	parseLineRange str = case split "-" str of
		[] -> 'Data.Error'.Error ["No linerange given"]
		[_] -> 'Data.Error'.Error ["No \"-\" found in linerange \"" <+ str <+ "\""]
		[start, end] -> parseLineRange` start end
		_ -> 'Data.Error'.Error ["Multiple \"-\" found in linerange \"" <+ str <+ "\""]
	where
		parseLineRange` :: !String !String -> 'Data.Error'.MaybeError [String] LineRange
		parseLineRange` "" "" = 'Data.Error'.Ok { Range | start = ?None, end = ?None }
		parseLineRange` "" end = case parseString end of
			?None -> 'Data.Error'.Error ["Could not parse \"" <+ end <+ "\" in linerange"]
			?Just end -> 'Data.Error'.Ok { Range | start = ?None, end = ?Just end }
		parseLineRange` start "" = case parseString start of
			?None -> 'Data.Error'.Error ["Could not parse \"" <+ start <+ "\" in linerange"]
			?Just start -> 'Data.Error'.Ok { Range | start = ?Just start, end = ?None }
		parseLineRange` start end = case (parseString start, parseString end) of
			(?None, _) -> 'Data.Error'.Error ["Could not parse \"" <+ start <+ "\" in linerange"]
			(_, ?None) -> 'Data.Error'.Error ["Could not parse \"" <+ end <+ "\" in linerange"]
			(?Just start, ?Just end) -> 'Data.Error'.Ok { Range | start = ?Just start, end = ?Just end }

showDiagnostic :: !Bool !Diagnostic -> String
showDiagnostic c d = concat
	[ if c (colorCode d.Diagnostic.severity) ""
	, toString d.Diagnostic.severity
	, ": "
	, if c clearColorCode ""
	, toString d.source
	, "."
	, toString d.dCode
	, ": "
	, toString d.range
	, ": "
	, toString d.message
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

instance toString DiagnosticSource
where
	toString TrailingWhitespacePass = "whitespace"
	toString _ = "MISSING_TO_STRING_FOR_SOURCE"

instance toString (Range t) | toString t
where
	toString { Range | start, end } = concat3 (toString start) "-" (toString end)

instance toString Position
where
	toString { Position | line, character } = concat3 (toString line) "," (toString character)

Start :: !*World -> *World
Start world
	# ([prog:args], world) = getCommandLine world
	# opts = parseOptions optionDesciption args gDefault{|*|}
	| 'Data.Error'.isError opts
		= exit (join "\n" $ 'Data.Error'.fromError opts) world
	# opts = 'Data.Error'.fromOk opts
	| invalidOptions opts
		= exit (concat5 "Usage: " prog usage "\n\n" (showHelpText (helpText optionDesciption))) world
	= execIO (startIO opts) world
where
	exit :: String *World -> *World
	exit error w = snd $ fclose (stderr <<< error <<< "\n") $ setReturnCode 1 w

startIO :: !Options -> IO ()
startIO opts=:{file = ?Just file}
	= putStrLn file >>| withWorld (runPassesFile (createConfiguration opts) file) >>= showResult opts

showResult :: !Options !('Data.Error'.MaybeError FileError [Diagnostic]) -> IO ()
showResult _ ('Data.Error'.Error fileError) =
	putStrLn (toString fileError) >>|
	withWorld (\w -> ((), setReturnCode 1 w))
showResult {color} ('Data.Error'.Ok diagnostics) =
	mapM_ (putStrLn o showDiagnostic color) diagnostics

createConfiguration :: !Options -> Configuration
createConfiguration {lines} =
	{ defaultConfiguration
	& lineRanges = lines
	}

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
