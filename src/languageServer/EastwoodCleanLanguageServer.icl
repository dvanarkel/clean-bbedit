module EastwoodCleanLanguageServer

import StdEnv
import StdOverloadedList

from Data.Error import instance Functor (MaybeError e), fromOk
import qualified Data.Error
import qualified Data.Map
import Data.Func
import Data.Functor
import Data.List
import Data.Maybe
import Data.Tuple
import System.Environment
import System.File
import System.FilePath
import System.OS
import Text
import Text.GenJSON
import Text.URI

import Text.YAML

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
import LSP.MessageParams
import LSP.ShowMessageParams
import LSP.NotificationMessage
import LSP.RequestMessage
import LSP.ResponseMessage
import LSP.ServerCapabilities
import LSP.TextDocumentIdentifier
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

Start :: !*World -> *World
Start w = serve capabilities cleanLanguageServer w

capabilities :: ServerCapabilities
capabilities =
	{ ServerCapabilities
	| textDocumentSync = {openClose = True, save = True}
	}

:: EastwoodState =
	{ workspaceFolders :: ![!FilePath]
	, compilerSettings :: !CompilerSettings
	}

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
cleanLanguageServer = {onInitialize = onInitialize, onRequest = onRequest, onNotification = onNotification}

onInitialize :: !InitializeParams !*World -> (!MaybeError String EastwoodState, !*World)
onInitialize {rootPath, rootUri, workspaceFolders} world =
	appFst (fmap toEastwoodState) $
	fetchConfig workspace world
where
	toEastwoodState compilerSettings =
		{ EastwoodState
		| workspaceFolders = workspace
		, compilerSettings = compilerSettings
		}

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
	| isNone mbConfigPath = ('Data.Error'.Error ("Could not find " +++ PROJECT_FILENAME), world)
	# configPath = fromJust mbConfigPath </> PROJECT_FILENAME
	# (mbConfig, world) = readFile configPath world
	  config = 'Data.Error'.fromOk mbConfig
	// Check if we could parse the yml file
	| 'Data.Error'.isError mbConfig =
		( 'Data.Error'.Error $ concat4
			"Cannot get project settings from " configPath
			": " (toString $ 'Data.Error'.fromError mbConfig)
		, world)
	// Parse the YAML, ignore warnings
	# mbYML = loadYAML coreSchema config
	  config = fst $ 'Data.Error'.fromOk mbYML
	  // Interpret the paths relative to the path of the configuration file
	  config & paths = [takeDirectory configPath </> p \\ p <- config.paths]
	| 'Data.Error'.isError mbYML =
		( 'Data.Error'.Error $ concat4
			"Invalid format of project file " configPath
			": " (toString $ 'Data.Error'.fromError mbYML)
		, world)
	// Get CLEAN_HOME
	# (mbCleanHome, world) = getEnvironmentVariable CLEAN_HOME_ENV_VAR world
	  cleanHome = fromJust mbCleanHome
	| isNone mbCleanHome =
		( 'Data.Error'.Error $ concat3
			"Could not get " CLEAN_HOME_ENV_VAR " environment variable"
		, world)
	# searchPaths = (libPathFor cleanHome <$> config.libraries) ++ config.paths
	# (fullSearchPaths, world) = mapSt getFullPathName searchPaths world
	# mbErr = firstSearchPathError searchPaths fullSearchPaths
	| isJust mbErr = ('Data.Error'.Error $ fromJust mbErr, world)
	# config =
		{ compilerPath = cleanHome </> EXE_PATH </> config.compiler
		, searchPaths = map 'Data.Error'.fromOk fullSearchPaths
		}
	| otherwise
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

onRequest :: !RequestMessage !EastwoodState !*World -> (!ResponseMessage, !EastwoodState, !*World)
onRequest {RequestMessage | id} st world = (errorResponse id, st, world)
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
where
	errorLogMessage :: !String -> NotificationMessage
	errorLogMessage message = showMessage {MessageParams| type = Error, message = message}

diagnosticsFor ::
	!TextDocumentIdentifier !EastwoodState !*World
	-> (!MaybeError String ([!NotificationMessage], [!'LSP.PublishDiagnosticsParams'.PublishDiagnosticsParams]), !*World)
diagnosticsFor {TextDocumentIdentifier| uri = uri=:{uriPath}} {compilerSettings} world
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
			# (notifications, diagnostics, world) = collectDiagnostics diagnostics world
			-> ('Data.Error'.Ok (notifications, diagnostics), world)
		error
			-> ('Data.Error'.liftError error, world)
where
	collectDiagnostics
		:: !('Data.Map'.Map FilePath [!Diagnostic]) !*World
		-> (![!NotificationMessage], ![!'LSP.PublishDiagnosticsParams'.PublishDiagnosticsParams], !*World)
	collectDiagnostics diagnostics world = 'Data.Map'.foldrWithKey` append ([!], [!], world) diagnostics
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

severityCorrespondingTo :: !'Eastwood.Diagnostic'.DiagnosticSeverity -> 'LSP.Diagnostic'.DiagnosticSeverity
severityCorrespondingTo 'Eastwood.Diagnostic'.Error       = 'LSP.Diagnostic'.Error
severityCorrespondingTo 'Eastwood.Diagnostic'.Warning     = 'LSP.Diagnostic'.Warning
severityCorrespondingTo 'Eastwood.Diagnostic'.Hint        = 'LSP.Diagnostic'.Hint
severityCorrespondingTo 'Eastwood.Diagnostic'.Information = 'LSP.Diagnostic'.Information
