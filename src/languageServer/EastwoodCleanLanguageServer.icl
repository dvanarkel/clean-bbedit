module EastwoodCleanLanguageServer

import StdEnv
import StdOverloadedList

from Data.Error import instance Functor (MaybeError e), fromOk, isError, :: MaybeError (Ok), fromError
from Data.Error import qualified :: MaybeError (Error)
import qualified Data.Error
import qualified Data.Map
import Data.Func
import Data.Either
import Data.Functor
import Data.List
import Data.Maybe
import Data.Tuple
import System.Environment
import System.File
import System.FilePath
import System.OS
from System.Process import :: ProcessResult {..}, callProcessWithOutput
import Text
import Text.GenJSON
import Text.URI
import Text.YAML
from Text.Unicode.UChar import isSymbol, :: UChar, instance fromChar UChar, instance toChar UChar, instance == UChar, instance toInt UChar
	, isAlphaNum, isPunctuation

from LSP.Diagnostic import qualified :: Diagnostic {..}, :: DiagnosticSeverity {..}
from LSP.Position import qualified :: Position {..}
from LSP.PublishDiagnosticsParams import qualified :: PublishDiagnosticsParams {..}
from LSP.Range import qualified :: Range {..}
import LSP
import LSP.BasicTypes
from LSP.DidOpenTextDocumentParams import qualified :: DidOpenTextDocumentParams {..}
from LSP.DidSaveTextDocumentParams import qualified :: DidSaveTextDocumentParams {..}
import LSP.InitializeParams
import LSP.Internal.Serialize
from LSP.MessageParams import :: MessageParams (..), :: MessageType (..)
import LSP.ShowMessageParams
import LSP.NotificationMessage
import LSP.RequestMessage
import LSP.ResponseMessage
import LSP.ServerCapabilities
import LSP.TextDocumentIdentifier
from LSP.Location import :: Location (..)
import qualified LSP.Position
import qualified LSP.PublishDiagnosticsParams
import qualified LSP.Range

from Eastwood.Diagnostic import :: DiagnosticSource
from Eastwood.Diagnostic import qualified :: Diagnostic {..}, :: DiagnosticSeverity
from Eastwood.Range import :: Range {..}, :: CharacterRange
from Eastwood.Range import qualified :: Position {..}
import qualified Eastwood.Diagnostic
import qualified Eastwood.Range
import Eastwood.Util.FileFinder
import Eastwood.Util.ModuleNameResolver

import Compiler

CLEAN_HOME_ENV_VAR :== "CLEAN_HOME"
EXE_PATH :== "lib/exe"
LIBS_PATH :== "lib"

PROJECT_FILENAME :== "Eastwood.yml"
README_LINK :== "https://gitlab.com/top-software/eastwood/-/blob/main/README.md"

Start :: !*World -> *World
Start w = serve capabilities cleanLanguageServer w

capabilities :: ServerCapabilities
capabilities =
	{ ServerCapabilities
	| textDocumentSync = {openClose = True, save = True}
	, declarationProvider = True
	}

:: EastwoodState = {workspaceFolders :: ![!FilePath]}

//* Shadow for `CompilerSettings` on which `gConstructFromYAML` can be derived.
:: CompilerSettingsConfig =
	{ compiler :: !FilePath
		//* Compiler's executable name (e.g. `cocl`, `cocl-itasks`), supposed
		//* to be found in `CLEAN_HOME/EXE_PATH`.
	, libraries :: ![String]
		//* Library names, found in `CLEAN_HOME/LIB_PATH`.
	, paths :: ![FilePath]
		//* Extra search paths, either absolute or relative to
		//* `PROJECT_FILENAME`.
	}

derive gConstructFromYAML CompilerSettingsConfig

cleanLanguageServer :: LanguageServer EastwoodState
cleanLanguageServer = { onInitialize = onInitialize, onRequest = onRequest, onNotification = onNotification}

onInitialize :: !InitializeParams !*World -> (!MaybeError String EastwoodState, !*World)
onInitialize {rootPath, rootUri, workspaceFolders} world
	= ('Data.Error'.Ok {EastwoodState|workspaceFolders=workspace}, world)
where
	workspace :: [!FilePath]
	workspace = case workspaceFolders of
		?Just folders -> [|uriToString uri \\ {WorkspaceFolder | uri} <|- folders]
		?None -> case rootUri of
			?Just uri -> [|uriToString uri]
			?None -> case rootPath of
				?Just path -> [|path]
				?None -> [|]
	where
		// Strip the file:// prefix; this is needed for `readFile` etc. to work
		uriToString uri = toString {uri & uriScheme = ?None}

fetchConfig :: ![!FilePath] !*World -> (!'Data.Error'.MaybeError String CompilerSettings, !*World)
fetchConfig workspaceFolders world
	# (mbConfigPath, world) = findSearchPath PROJECT_FILENAME workspaceFolders world
	| isNone mbConfigPath
		= ('Data.Error'.Error $
			concat
				[ "Could not find the "
				, PROJECT_FILENAME
				, " project configuration file in the workspace folder. Please create the file in the workspace's root folder. The expected format of the "
				, PROJECT_FILENAME
				, " file is described in "
				, README_LINK
				, "."
				]
			, world)
	# configPath = fromJust mbConfigPath </> PROJECT_FILENAME
	# (mbConfig, world) = readFile configPath world
	// Check if the project file could be read.
	| 'Data.Error'.isError mbConfig =
		('Data.Error'.Error $
			concat4 "Cannot read project file found at " configPath ": " (toString $ 'Data.Error'.fromError mbConfig)
		, world)
	# config = 'Data.Error'.fromOk mbConfig
	// Parse the YAML, ignore warnings
	# mbYML = loadYAML coreSchema config
	// Check if the YML could be parsed.
	| 'Data.Error'.isError mbYML
		=
		( 'Data.Error'.Error $
			concat
				[ "Invalid format of project file "
				, configPath
				, ": "
				, (toString $ 'Data.Error'.fromError mbYML)
				, ". The expected format of the project file is described in "
				, README_LINK
				]
		, world)
	# config = fst $ 'Data.Error'.fromOk mbYML
	// Interpret the paths relative to the path of the configuration file.
	// The config file path is included by default, so that a developer doesn't have to explicitly add `.` in the config
	// file to include the root directory.
	# config & paths = [takeDirectory configPath : [takeDirectory configPath </> p \\ p <- config.paths]]
	// Get CLEAN_HOME
	# (mbCleanHome, world) = getEnvironmentVariable CLEAN_HOME_ENV_VAR world
	  cleanHome = fromJust mbCleanHome
	| isNone mbCleanHome =
		( 'Data.Error'.Error $ concat3 "Could not get " CLEAN_HOME_ENV_VAR " environment variable", world)
	# searchPaths = (libPathFor cleanHome <$> config.libraries) ++ config.paths
	# (fullSearchPaths, world) = mapSt getFullPathName searchPaths world
	# mbErr = firstSearchPathError searchPaths fullSearchPaths
	| isJust mbErr = ('Data.Error'.Error $ fromJust mbErr, world)
	# config =
		{ compilerPath = cleanHome </> EXE_PATH </> config.compiler
		, searchPaths = map 'Data.Error'.fromOk fullSearchPaths
		}
	= ('Data.Error'.Ok config, world)
where
	libPathFor :: !FilePath !FilePath -> FilePath
	libPathFor cleanHome lib = cleanHome </> LIBS_PATH </> lib

	firstSearchPathError :: ![FilePath] ![MaybeOSError FilePath] -> ?String
	firstSearchPathError paths fullPaths = case filter ('Data.Error'.isError o snd) $ zip2 paths fullPaths of
		[] -> ?None
		[(path, 'Data.Error'.Error (_, error)):_] -> ?Just $ concat
			[ "Failed to find full path of ", path
			, " mentioned in ", PROJECT_FILENAME
			, ": ", error
			]

import StdDebug
import Text.GenJSON

onRequest :: !RequestMessage !EastwoodState !*World -> (!ResponseMessage, !EastwoodState, !*World)
onRequest msg=:{RequestMessage | id, method} st world =
	case method of
		"textDocument/declaration" = onGotoDeclaration msg st world
		_ = (errorResponse id, st, world)
where
	errorResponse :: !RequestId -> ResponseMessage
	errorResponse id =
		{ ResponseMessage
		| id = ?Just id
		, result = ?None
		, error = ?Just
			{ ResponseError
			| errorCode = InternalError
			, message = "This language server is still work in progress."
			, data = ?None
			}
		}

onNotification :: !NotificationMessage !EastwoodState !*World -> (![!NotificationMessage], !EastwoodState, !*World)
onNotification {NotificationMessage| method, params} st world
	| method == "textDocument/didSave" || method == "textDocument/didOpen"
		| isNone params
			= ([!errorLogMessage (concat3 "Missing argument for '" method "'.")], st, world)
		# (diags, world) = diagnosticsFor (textDocument (fromJust params)) st world
		= case diags of
			'Data.Error'.Ok (notifications, diags) =
				( notifications ++|
					[!notificationMessage "textDocument/publishDiagnostics" (?Just diag) \\ diag <|- diags]
				, st
				, world)
			'Data.Error'.Error err =
				([!errorLogMessage err], st, world)
		with
			textDocument params = case method of
				"textDocument/didSave" -> (deserialize params).'LSP.DidSaveTextDocumentParams'.textDocument
				"textDocument/didOpen" -> (deserialize params).'LSP.DidOpenTextDocumentParams'.textDocument
	| method == "textDocument/didClose"
		= ([!], st, world)
	| method == "$/setTrace"
		= ([!], st, world)
	| otherwise
		= ([!errorLogMessage $ concat3 "Unknown notification '" method "'."], st, world)

import Debug.Trace
import Data.GenEq

derive gEq Location, URI, 'Eastwood.Range'.Range, 'LSP.Range'.Range, 'LSP.Position'.Position, UInt

onGotoDeclaration :: !RequestMessage !EastwoodState !*World -> (!ResponseMessage, !EastwoodState, !*World)
onGotoDeclaration req=:{RequestMessage|id, params=(?Just json)} st=:{EastwoodState|workspaceFolders} world
	// Boilerplate to get the line (String) for which a declaration was requested.
	# {GotoDeclarationParams|textDocument={TextDocumentIdentifier|uri}, position} = deserialize json
	# (mbLines, world) = trace_n (toString uri) readFileLines uri.uriPath world
	| isError mbLines =
		( errorResponse id InternalError (concat3 "The file located at " uri.uriPath "was not found.")
		, st
		, world)
	# lines = fromOk mbLines
	# (UInt lineNr) = position.'LSP.Position'.line
	# mbLine = lines !? lineNr
	| isNone mbLine =
		( errorResponse
			id
			InternalError
			(concat4 "The file located at " uri.uriPath "does no longer contain line number " (toString lineNr))
		, st
		, world
		)
	// The line was found.
	# line = fromJust mbLine
	// NB: charNr does not mean column number, it is the number of the character within the line.
	// E.g tab = x cols 1 char.
	# charNr = position.'LSP.Position'.character
	// Parse the search term for which a declaration was requested.
	# mbSearchString = searchTermFor line charNr
	| isError mbSearchString = (fromError mbSearchString, st, world)
	// Using the searchString, execute grep to find the file names and line numbers of the definitions that match.
	# searchString = fromOk mbSearchString
	// CLEAN_HOME_ENV_VAR is also searched by grep so the value is retrieved.
	# (mbCleanHome, world) = getEnvironmentVariable CLEAN_HOME_ENV_VAR world
	| isNone mbCleanHome
		= (errorResponse id InternalError "Could not find CLEAN_HOME.", st, world)
	# cleanHomePath = fromJust mbCleanHome
	//* The grep typedef search string is adjusted to avoid finding imports using (?<!).
	# typeDefSearchStr = "(?<!import |, ):: " +++ searchString
	// -P enables perl regexp, -r recurses through all files, -n gives line number, --include \*.dcl makes sure only
	// dcl files are examined by grep.
	# (mbGrepResultTypeDef, world)
		= callProcessWithOutput "grep" ["-P", typeDefSearchStr, "-r", "-n", "--include", "\*.dcl", ".", cleanHomePath] ?None world
	| isError mbGrepResultTypeDef
		= (errorResponse id InternalError "grep failed when searching for type definitions.", st, world)
	# {stdout} = fromOk mbGrepResultTypeDef
	// List of lists of string containing the results, grep separates file name/line number by : and results by \n.
	// grep adds an empty newline at the end of the results which is removed by init.
	// The first two results are the file name and line number.
	// This is transformed into a list of tuples of filename and linenumber for convenience.
	# results = (\[fileName, lineNr] -> ([(fileName, toInt lineNr)])) o take 2 o split ":" <$> (init $ split "\n" stdout)
	//Grep returns relative paths but the client needs absolute path URIs so the absolute path is determined.
	# (mbSearchPath, world) = findSearchPath PROJECT_FILENAME workspaceFolders world
	| isNone mbSearchPath
		= (errorResponse id InternalError ("could not find absolute path of " +++ PROJECT_FILENAME), st, world)
	# searchPath = fromJust mbSearchPath
	# (mbAbsPath, world) = getFullPathName searchPath world
	| isError mbAbsPath =
		(errorResponse id InternalError ("could not find absolute path of " +++ PROJECT_FILENAME), st, world)
	# absPath = fromOk mbAbsPath
	// // For every tuple of fileName and lineNumber, a Location is generated to be sent back to the client.
	# locations = [! l \\ l <- catMaybes $ (fileAndLineToLocation absPath cleanHomePath) <$> flatten results !]
	// # funcDefSearchStr = searchString +++ " ::"
	= (locationResponse id locations, st, world)
where
	searchTermFor :: !String !UInt -> MaybeError ResponseMessage String
	searchTermFor line (UInt charNr)
		# firstUnicodeChar = fromChar $ select line charNr
		#! firstUnicodeChar = trace_n (toInt firstUnicodeChar) firstUnicodeChar
		| isSpecialSymbol firstUnicodeChar =
			'Data.Error'.Error $
				errorResponse id InternalError "it is not possible to go to the definition of a special syntax symbol."
		// If the first char is a space, comma, \n, or \t, go backwards
		// When goto definition is pressed when a whole term is selected the character ends up being the first char
		// after the term, the same holds when attempting to go to the declaration when selecting these characters.
		# lookBackCharacters = fromChar <$> [' ', ',', '\n', '\t']
		| elem firstUnicodeChar lookBackCharacters = trace_n "lookup" searchTermFor line (UInt (charNr - 1))
		| isSymbol firstUnicodeChar || isAlphaNum firstUnicodeChar || isPunctuation firstUnicodeChar
			# stopPredicate =
				if (isSymbol firstUnicodeChar || isPunctuation firstUnicodeChar)
					symbolPuncStopPredicate
					alphaNumStopPredicate
			// Parse backwards and forwards until a char that triggers the stopPredicate is found
			// or when we run out of chars to parse.
			# searchTerm =
				toString $
				(Reverse $ parseSearchTerm line stopPredicate (Reverse [!0..charNr-1!])) ++|
				parseSearchTerm line stopPredicate [!charNr..size line!]
			= Ok searchTerm
		= 'Data.Error'.Error $
			errorResponse
			id
			InternalError
			("Unrecognised char: " +++ (toString $ toInt firstUnicodeChar))
	where
		parseSearchTerm :: !String !(UChar -> Bool) ![!Int!] -> [!Char!]
		parseSearchTerm line stopPredicate indexes = parseSearchTerm` line filter indexes [!!]
		where
			parseSearchTerm` line filter [!i:is!] acc
				# uChar = fromChar $ select line i
				// If there is a character that adheres to the stop filter, break out of the recursion.
				| stopPredicate uChar = Reverse acc
				= parseSearchTerm` line filter is [!toChar uChar:acc!]
			// If there are no indexes left, we return the accumlator of characters for which the filter holds.
			parseSearchTerm` _ _ [!!] acc = Reverse acc

		symbolPuncStopPredicate :: !UChar -> Bool
		symbolPuncStopPredicate uc = not (isSymbol uc || isPunctuation uc) || isSpecialSymbol uc

		alphaNumStopPredicate :: !UChar -> Bool
		alphaNumStopPredicate uc = not (isAlphaNum uc)

		// Not possible to retrieve a declaration for these symbols are they are special syntax characters.
		// Not strict because there is currently no Elem in StdOverloadedList.
		specialSymbols :: [UChar]
		specialSymbols = map fromChar ['(', ')', '{', '}', '[', ']', ';', '\"', '\'', '_', 'c']

		isSpecialSymbol :: !UChar -> Bool
		isSpecialSymbol uc = elem uc specialSymbols

	grepTypeDefintions :: !String -> [!Location!]
	grepTypeDefintions searchString = [!!]

	grepFunDefinitions :: !String -> [!Location!]
	grepFunDefinitions searchString = [!!]

	errorResponse :: !RequestId !ErrorCode !String -> ResponseMessage
	errorResponse id err msg =
		{ ResponseMessage
		 | id = ?Just id
		, result = ?None
		, error = ?Just
			{ ResponseError
			| errorCode = err
			, message = msg
			, data = ?None
			}
		}

	locationResponse :: !RequestId ![!Location!] -> ResponseMessage
	locationResponse id locations=
		{ ResponseMessage
		| id = ?Just id
		, result =
			?Just $ serialize locations
		, error = ?None
		}

	fileAndLineToLocation :: !FilePath !FilePath !(!String, !Int) -> ?Location
	fileAndLineToLocation absPath cleanHomePath (fileName, lineNr)
		// Grep returns relative paths, vscode needs absolute paths so the absolute path is determined.
		# (mbRootPath, world) = findSearchPath PROJECT_FILENAME workspaceFolders
		#! fileName = trace_n fileName fileName
		// Files in CLEAN_HOME already are in absolute path form, therefore we do not add absPath in this case.
		# fileUri = parseURI $ "file://"  </> (if (startsWith cleanHomePath fileName) (fileName) (absPath </> fileName))
		| isNone fileUri = ?None
		= ?Just $
			{ Location
			| uri = fromJust fileUri
			, range = rangeCorrespondingTo
				{ 'Eastwood.Range'.Range
				| start={'Eastwood.Range'.Position|'Eastwood.Range'.line=lineNr-1, 'Eastwood.Range'.character=0}
				, end={'Eastwood.Range'.Position|'Eastwood.Range'.line=lineNr-1, 'Eastwood.Range'.character=0}
				}
			}

:: GotoDeclarationParams = { textDocument :: !TextDocumentIdentifier, position :: !'LSP.Position'.Position}

// (#,) infixl 3 :: !Int !Int -> Bool
// (#,) i1 i2 = True

derive gLSPJSONDecode GotoDeclarationParams
derive gLSPJSONEncode GotoDeclarationParams

errorLogMessage :: !String -> NotificationMessage
errorLogMessage message = showMessage {MessageParams| type = Error, message = message}

diagnosticsFor ::
	!TextDocumentIdentifier !EastwoodState !*World
	-> (!MaybeError String ([!NotificationMessage], [!'LSP.PublishDiagnosticsParams'.PublishDiagnosticsParams]), !*World)
diagnosticsFor {TextDocumentIdentifier| uri = uri=:{uriPath}} {EastwoodState|workspaceFolders} world
	# (mbCompilerSettings, world) = fetchConfig workspaceFolders world
	| 'Data.Error'.isError mbCompilerSettings
		= ('Data.Error'.Ok $ ([!errorLogMessage $ 'Data.Error'.fromError mbCompilerSettings], [!]), world)
	# compilerSettings = fromOk mbCompilerSettings
	# (mbModuleName, world) = resolveModuleName uriPath compilerSettings.searchPaths world
	  moduleName = fromOk mbModuleName
	| 'Data.Error'.isError mbModuleName
		// Create a ad hoc diagnostic for failing to determine the module name:
		// we want to return an `Ok` so that we send diagnostics for this file,
		// instead of tracing an error message with `showMessage`.
		# diagnostic =
			{ 'LSP.PublishDiagnosticsParams'.uri = uri
			, 'LSP.PublishDiagnosticsParams'.diagnostics =
				[|{'LSP.Diagnostic'.range =
					{ 'LSP.Range'.start = {'LSP.Position'.line = uint 0, 'LSP.Position'.character = uint 0}
					, 'LSP.Range'.end = {'LSP.Position'.line = uint 0, 'LSP.Position'.character = uint 999999}
					}
				, 'LSP.Diagnostic'.severity = ?Just 'LSP.Diagnostic'.Error
				, 'LSP.Diagnostic'.codeDescription = ?None
				, 'LSP.Diagnostic'.source = ?None
				, 'LSP.Diagnostic'.message = "Failed to determine module name: " +++ 'Data.Error'.fromError mbModuleName
				, 'LSP.Diagnostic'.tags = [|]
				, 'LSP.Diagnostic'.relatedInformation = [|]
				, 'LSP.Diagnostic'.data = JSONNull
				}]
			}
		=
			( 'Data.Error'.Ok ([!], [!diagnostic])
			, world)
	# (diagnostics, world) = runCompiler uriPath moduleName compilerSettings world
	= case diagnostics of
		'Data.Error'.Ok diagnostics
			# (notifications, diagnostics, world) = collectDiagnostics compilerSettings diagnostics world
			-> ('Data.Error'.Ok (notifications, diagnostics), world)
		error
			-> ('Data.Error'.liftError error, world)
where
	collectDiagnostics
		:: !CompilerSettings !('Data.Map'.Map FilePath [!Diagnostic]) !*World
		-> (![!NotificationMessage], ![!'LSP.PublishDiagnosticsParams'.PublishDiagnosticsParams], !*World)
	collectDiagnostics compilerSettings diagnostics world
		= 'Data.Map'.foldrWithKey` append ([!], [!], world) diagnostics
	where
		append fileName diagnosticsForFile (notifications, collectedDiagnostics, world)
			// We need the fileName with `OS_PATH_SEPARATOR`s for
			// `findSearchPath`, but afterwards convert it to use `/` when
			// generating a URI.
			# (mbPath, world) = findSearchPath fileName compilerSettings.searchPaths world
			| isNone mbPath
				# notifications =
					[! showMessage {type = Warning, message = "Failed to find path for " +++ fileName}
					: notifications
					]
				= (notifications, collectedDiagnostics, world)
			# uriPath = fromJust mbPath </> {if (c == OS_PATH_SEPARATOR) '/' c \\ c <-: fileName}
			// Use getFullPathName to remove . and .. from paths (mainly to
			// ease testing because it gives a normalized form for paths)
			# (mbRealPath, world) = getFullPathName uriPath world
			# uriPath = fromMaybe uriPath $ 'Data.Error'.error2mb mbRealPath
			# collectedDiagnostics =
				[!
					{ 'LSP.PublishDiagnosticsParams'.uri = {uri & uriPath = uriPath}
					, 'LSP.PublishDiagnosticsParams'.diagnostics = Map lspDiagnosticFor diagnosticsForFile
					}
				: collectedDiagnostics
				]
			= (notifications, collectedDiagnostics, world)

	lspDiagnosticFor :: !'Eastwood.Diagnostic'.Diagnostic -> 'LSP.Diagnostic'.Diagnostic
	lspDiagnosticFor diagnostic =
		{ 'LSP.Diagnostic'.range = rangeCorrespondingTo diagnostic.'Eastwood.Diagnostic'.range
		, 'LSP.Diagnostic'.severity = ?Just $ severityCorrespondingTo diagnostic.'Eastwood.Diagnostic'.severity
		, 'LSP.Diagnostic'.codeDescription = ?None
		, 'LSP.Diagnostic'.source = ?None
		, 'LSP.Diagnostic'.message = diagnostic.'Eastwood.Diagnostic'.message
		, 'LSP.Diagnostic'.tags = [!]
		, 'LSP.Diagnostic'.relatedInformation = [!]
		, 'LSP.Diagnostic'.data = JSONNull
		}

rangeCorrespondingTo :: !('Eastwood.Range'.Range 'Eastwood.Range'.Position) -> 'LSP.Range'.Range
rangeCorrespondingTo range =
	{'LSP.Range'.Range
		| 'LSP.Range'.start = positionCorrespondingTo range.'Eastwood.Range'.Range.start
		, 'LSP.Range'.end = positionCorrespondingTo range.'Eastwood.Range'.Range.end
	}
where
	positionCorrespondingTo :: !'Eastwood.Range'.Position -> 'LSP.Position'.Position
	positionCorrespondingTo {'Eastwood.Range'.Position| 'Eastwood.Range'.line, 'Eastwood.Range'.character} =
		{'LSP.Position'.Position
			| 'LSP.Position'.line = uint line
			, 'LSP.Position'.character = uint character
		}

instance toString YAMLErrorWithLocations where
	toString {error, locations} =
		concat5
			"Error occurred while constructing YAML: "
			(toString error)
			"."
			(if (isEmpty locations) ("") ("The following hints were provided for solving the error: "))
			(join ". " (map (\l -> "Error occurred " +++ toString l) locations))

instance toString ErrorLocation where
	toString (ADT a) = concat3 "while parsing ADT \"" a "\""
	toString (Constructor c) = concat3 "while parsing constructor \"" c "\""
	toString (Record r) = concat3 "while parsing record \"" r "\""
	toString (Field f) = concat3 "while parsing field \"" f "\""
	toString (SequenceIndex i) = "at sequence index " +++ (toString i)

severityCorrespondingTo :: !'Eastwood.Diagnostic'.DiagnosticSeverity -> 'LSP.Diagnostic'.DiagnosticSeverity
severityCorrespondingTo 'Eastwood.Diagnostic'.Error       = 'LSP.Diagnostic'.Error
severityCorrespondingTo 'Eastwood.Diagnostic'.Warning     = 'LSP.Diagnostic'.Warning
severityCorrespondingTo 'Eastwood.Diagnostic'.Hint        = 'LSP.Diagnostic'.Hint
severityCorrespondingTo 'Eastwood.Diagnostic'.Information = 'LSP.Diagnostic'.Information
