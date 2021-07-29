module EastwoodCleanLanguageServer

import StdEnv
import StdOverloadedList

from Data.Error import fromOk
import qualified Data.Error
import qualified Data.Map
import Data.Func
import Data.Maybe
import Text
import Text.GenJSON
import Text.URI
import System.FilePath

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

import Compiler

Start :: !*World -> *World
Start w = serve capabilities cleanLanguageServer w

capabilities :: ServerCapabilities
capabilities =
	{ ServerCapabilities
	| textDocumentSync = {openClose = True, save = True}
	}

:: EastwoodState =
	{ workspaceFolders :: ![!FilePath]
	}

cleanLanguageServer :: LanguageServer EastwoodState
cleanLanguageServer = {onInitialize = onInitialize, onRequest = onRequest, onNotification = onNotification}

onInitialize :: !InitializeParams -> EastwoodState
onInitialize {rootPath, rootUri, workspaceFolders} =
	{ EastwoodState
	| workspaceFolders = workspace
	}
where
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
			'Data.Error'.Ok diags =
				([!notificationMessage "textDocument/publishDiagnostics" (?Just diag) \\ diag <|- diags], st, world)
			'Data.Error'.Error err =
				([!errorLogMessage err], st, world)
		with
			textDocument params = case method of
				"textDocument/didSave" -> (deserialize params).'LSP.DidSaveTextDocumentParams'.textDocument
				"textDocument/didOpen" -> (deserialize params).'LSP.DidOpenTextDocumentParams'.textDocument
	| otherwise
		= ([!errorLogMessage $ concat3 "Unknown notification '" method "'."], st, world)
where
	errorLogMessage :: !String -> NotificationMessage
	errorLogMessage message = showMessage {MessageParams| type = Error, message = message}

diagnosticsFor ::
	!TextDocumentIdentifier !EastwoodState !*World
	-> (!(MaybeError String [!'LSP.PublishDiagnosticsParams'.PublishDiagnosticsParams]), !*World)
diagnosticsFor {TextDocumentIdentifier| uri = uri=:{uriPath}} eastwoodState world
	# (diagnostics, world) = runCompiler uriPath eastwoodState.EastwoodState.workspaceFolders world
	= ( case diagnostics of
			'Data.Error'.Ok diagnostics =
				'Data.Error'.Ok
					[!	let uriForFile = takeDirectory uriPath </> fileName in
						{ 'LSP.PublishDiagnosticsParams'.uri         = {uri & uriPath = uriForFile}
						, 'LSP.PublishDiagnosticsParams'.diagnostics = Map lspDiagnosticFor diagnosticsForFile
						}
						\\ (fileName, diagnosticsForFile) <- 'Data.Map'.toList diagnostics
					]
			error =
				'Data.Error'.liftError error
		, world)
where
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
