module EastwoodCleanLanguageServer

import StdEnv
import StdOverloadedList

from Data.Error import fromOk, isError, :: MaybeError (Ok), fromError

import qualified Data.Error
import qualified Data.Map
import Data.Func
import Data.Functor
import Data.List
import Data.Maybe
import System.Environment
import System.FilePath
import System.OS
import Text
import Text.GenJSON
import Text.Encodings.UrlEncoding
import Text.URI

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

from Eastwood.Diagnostic import qualified :: Diagnostic {..}, :: DiagnosticSeverity
import qualified Eastwood.Diagnostic
import Eastwood.Util.FileFinder
import Eastwood.Util.ModuleNameResolver

import Compiler
import Config
import Constants
import GotoDeclaration
import GotoDefinition
import Util

Start :: !*World -> *World
Start w = serve capabilities cleanLanguageServer w

capabilities :: ServerCapabilities
capabilities =
	{ ServerCapabilities
	| textDocumentSync = {openClose = True, save = True}
	, declarationProvider = True
	, definitionProvider = True
	}

compilerSettingsConfigToCompilerSettings :: !CompilerSettingsConfig !*World -> (!MaybeError String CompilerSettings, !*World)
compilerSettingsConfigToCompilerSettings {compiler, libraries, paths} world
	# (mbCleanHome, world) = getEnvironmentVariable CLEAN_HOME_ENV_VAR world
	  cleanHome = fromJust mbCleanHome
	| isNone mbCleanHome =
		( 'Data.Error'.Error $ concat3 "Could not get " CLEAN_HOME_ENV_VAR " environment variable", world)
	# searchPaths = (libPathFor cleanHome <$> libraries) ++ paths
	# (fullSearchPaths, world) = mapSt getFullPathName searchPaths world
	# mbErr = firstSearchPathError searchPaths fullSearchPaths
	| isJust mbErr = ('Data.Error'.Error $ fromJust mbErr, world)
	= (Ok {compilerPath= cleanHome </> EXE_PATH </> compiler, searchPaths=fromOk <$> fullSearchPaths}, world)
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

cleanLanguageServer :: LanguageServer EastwoodState
cleanLanguageServer = {onInitialize = onInitialize, onRequest = onRequest, onNotification = onNotification}

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

onRequest :: !RequestMessage !EastwoodState !*World -> (!ResponseMessage, !EastwoodState, !*World)
onRequest msg=:{RequestMessage | id, method} st world =
	case method of
		"textDocument/declaration"
			# (response, world) = onGotoDeclaration msg st world
			= (response, st, world)
		"textDocument/definition"
			# (response, world) = onGotoDefinition msg st world
			= (response, st, world)
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
			, message = "This message type is not (yet) supported."
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
	| method == "$/cancelRequest"
		= ([!], st, world)
	| otherwise
		= ([!errorLogMessage $ concat3 "Unknown notification '" method "'."], st, world)

errorLogMessage :: !String -> NotificationMessage
errorLogMessage message = showMessage {MessageParams| type = Error, message = message}

diagnosticsFor ::
	!TextDocumentIdentifier !EastwoodState !*World
	-> (!MaybeError String ([!NotificationMessage], [!'LSP.PublishDiagnosticsParams'.PublishDiagnosticsParams]), !*World)
diagnosticsFor {TextDocumentIdentifier| uri = uri=:{uriPath}} {EastwoodState|workspaceFolders} world
	// Decode url since Clean expects filepaths that are not URL encoded while LSP supplies URL encoded filepaths.
	# uriPath = urlDecode uriPath
	# (mbCompilerSettingsConfig, world) = fetchConfig workspaceFolders world
	| 'Data.Error'.isError mbCompilerSettingsConfig
		= ('Data.Error'.Ok $ ([!errorLogMessage $ 'Data.Error'.fromError mbCompilerSettingsConfig], [!]), world)
	# (mbCompilerSettings, world) = compilerSettingsConfigToCompilerSettings (fromOk mbCompilerSettingsConfig) world
	| 'Data.Error'.isError mbCompilerSettings
		= ('Data.Error'.Ok ([! errorLogMessage $ 'Data.Error'.fromError mbCompilerSettings], [!]), world)
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

severityCorrespondingTo :: !'Eastwood.Diagnostic'.DiagnosticSeverity -> 'LSP.Diagnostic'.DiagnosticSeverity
severityCorrespondingTo 'Eastwood.Diagnostic'.Error       = 'LSP.Diagnostic'.Error
severityCorrespondingTo 'Eastwood.Diagnostic'.Warning     = 'LSP.Diagnostic'.Warning
severityCorrespondingTo 'Eastwood.Diagnostic'.Hint        = 'LSP.Diagnostic'.Hint
severityCorrespondingTo 'Eastwood.Diagnostic'.Information = 'LSP.Diagnostic'.Information
