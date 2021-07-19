module EastwoodLint

import StdEnv
import StdMaybe

import Control.Applicative
from Control.Monad import >>=, >>|, class Monad(bind), mapM, mapM_
from Data.Error import instance <*> (MaybeError a), instance Functor (MaybeError a), instance Monad (MaybeError a)
from Data.Error import instance pure (MaybeError a)
import qualified Data.Error
from Data.Func import $
from Data.Functor import <$>, class Functor(fmap)
import Data.GenDefault
from Data.List import intersperse
from Data.Maybe import maybe
from System.CommandLine import getCommandLine, setReturnCode
from System.File import :: FileError, instance toString FileError
from System.FilePath import dropDirectory, :: FilePath
import System.IO
import System.Options
import Text
import Text.GenParse

import Eastwood
import Eastwood.Configuration
import Eastwood.Diagnostic
from Eastwood.Pass.BasicValueCAFs import :: BasicValueCAFsConfiguration{..}, BasicValueCAFsPass
from Eastwood.Pass.TrailingWhitespace import :: TrailingWhitespaceConfiguration{..}, TrailingWhitespacePass
import Eastwood.Range

RED :== "\x1b[31m"
YELLOW :== "\x1b[33m"
BLUE :== "\x1b[34m"
CYAN :== "\x1b[36m"
CLEAR :== "\x1b[0m"

:: Options =
	{ color :: !Bool
	, werror :: !Bool
	, lines :: ![LineRange]
	, file :: !?FilePath
	}
derive gDefault ?, Range

gDefault{|Options|} =
	{ Options
	| color = True
	, werror = False
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
	, Flag "--Werror"
		(\opts -> 'Data.Error'.Ok {opts & werror = True})
		"Treat all warnings as errors"
	, Shorthand "-l" "--lines" $ Option
		"--lines"
		(\ls opts -> (\ls -> {opts & lines = ls}) <$> (mapM parseLineRange $ split "," ls))
		"LINE_RANGES"
		"Line ranges that should be considered (e.g. \"1-2,5-\")"
	, Operand True
		(\fp opts -> ?Just $ 'Data.Error'.Ok {opts & file = ?Just fp})
		"FILE"
		"The file on which the linter should be run"
	]
where
	parseLineRange :: String -> 'Data.Error'.MaybeError [String] LineRange
	parseLineRange str = case split "-" str of
		[] -> 'Data.Error'.Error ["No line range given"]
		[_] -> 'Data.Error'.Error ["No \"-\" found in line range \"" <+ str <+ "\""]
		[start, end] -> parseLineRange` start end
		_ -> 'Data.Error'.Error ["Multiple \"-\" found in line range \"" <+ str <+ "\""]
	where
		parseLineRange` :: !String !String -> 'Data.Error'.MaybeError [String] LineRange
		parseLineRange` "" "" = 'Data.Error'.Ok { Range | start = ?None, end = ?None }
		parseLineRange` "" end = case parseString end of
			?None -> 'Data.Error'.Error ["Could not parse \"" <+ end <+ "\" in line range"]
			?Just end -> 'Data.Error'.Ok { Range | start = ?None, end = ?Just end }
		parseLineRange` start "" = case parseString start of
			?None -> 'Data.Error'.Error ["Could not parse \"" <+ start <+ "\" in line range"]
			?Just start -> 'Data.Error'.Ok { Range | start = ?Just start, end = ?None }
		parseLineRange` start end = case (parseString start, parseString end) of
			(?None, _) -> 'Data.Error'.Error ["Could not parse \"" <+ start <+ "\" in line range"]
			(_, ?None) -> 'Data.Error'.Error ["Could not parse \"" <+ end <+ "\" in line range"]
			(?Just start, ?Just end) -> 'Data.Error'.Ok { Range | start = ?Just start, end = ?Just end }

showEastwoodDiagnostic :: !Options !Diagnostic -> String
showEastwoodDiagnostic {color, file} d
	# file = maybe "" (\f -> dropDirectory f +++ ":") file
	= concat
		[ if color (colorCode d.Diagnostic.severity) ""
		, toString d.Diagnostic.severity
		, ": "
		, if color CLEAR ""
		, toString d.source
		, "."
		, toString d.dCode
		, ": "
		, file
		, toString d.range
		, ": "
		, toString d.message
		]
where
	colorCode :: !DiagnosticSeverity -> String
	colorCode Error = RED
	colorCode Warning = YELLOW
	colorCode Information = BLUE
	colorCode Hint = CYAN

instance toString DiagnosticSeverity
where
	toString Error = "Error"
	toString Warning = "Warning"
	toString Information = "Info"
	toString Hint = "Hint"

instance toString DiagnosticSource
where
	toString BasicValueCAFsPass = "basic-value-caf"
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
	= withWorld (runPassesFile (createConfiguration opts) file)
	>>= \diagnostics -> case diagnostics of
		'Data.Error'.Error fileError -> putStrLn (toString fileError) >>| withWorld (\w -> ((), setReturnCode 1 w))
		'Data.Error'.Ok diagnostics
			#! diagnostics = handleWError opts.werror diagnostics
			-> showEastwoodDiagnostics opts diagnostics
			>>| returnCode diagnostics
where
	handleWError :: !Bool ![Diagnostic] -> [Diagnostic]
	handleWError False ds = ds
	handleWError True ds = map asError ds
	where
		asError :: !Diagnostic -> Diagnostic
		asError d = {Diagnostic | d & severity = Error}

	returnCode :: [Diagnostic] -> IO ()
	returnCode ds = if (any isError ds) setErrorCode (pure ())
	where
		setErrorCode :: IO ()
		setErrorCode = withWorld (\w -> ((), setReturnCode 1 w))

		isError :: !Diagnostic -> Bool
		isError {Diagnostic | severity = Error} = True
		isError _ = False

showEastwoodDiagnostics :: !Options ![Diagnostic] -> IO ()
showEastwoodDiagnostics opts diagnostics =
	mapM_ (putStrLn o showEastwoodDiagnostic opts) diagnostics

createConfiguration :: !Options -> Configuration
createConfiguration {lines} =
	{ defaultConfiguration
	& lineRanges = lines
	}

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
