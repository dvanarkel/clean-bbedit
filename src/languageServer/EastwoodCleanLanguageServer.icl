module EastwoodCleanLanguageServer

import StdDebug
import StdEnv
import StdOverloadedList
import Data.Either
from Data.Error import fromOk
import Data.Func
import Data.Maybe
import Text
import Text.GenJSON
import Text.URI

import LSP
import LSP.BasicTypes
import LSP.DidSaveTextDocumentParams
import LSP.Internal.Serialize
import LSP.NotificationMessage
import qualified LSP.PublishDiagnosticsParams
from LSP.PublishDiagnosticsParams import qualified :: PublishDiagnosticsParams {..}
import LSP.ResponseMessage
import LSP.RequestMessage
import LSP.ServerCapabilities
import LSP.TextDocumentIdentifier
from LSP.Diagnostic import qualified :: Diagnostic {..}, :: DiagnosticSeverity {..}
import qualified LSP.Range
from LSP.Range import qualified :: Range {..}
import qualified LSP.Position
from LSP.Position import qualified :: Position {..}

import qualified Eastwood.Diagnostic
from Eastwood.Diagnostic import :: DiagnosticSource
from Eastwood.Diagnostic import qualified :: Diagnostic {..}, :: DiagnosticSeverity
import qualified Eastwood.Range
from Eastwood.Range import :: Range {..}, :: CharacterRange
from Eastwood.Range import qualified :: Position {..}

import Compiler

Start :: !*World -> *World
Start w = serve capabilities cleanLanguageServer w

capabilities :: ServerCapabilities
capabilities =
	{ ServerCapabilities
	| textDocumentSync = {save = True}
	}

cleanLanguageServer :: LanguageServer ()
cleanLanguageServer = {initialState = (), onRequest = onRequest, onNotification = onNotification}

onRequest :: !RequestMessage !() !*World -> (!ResponseMessage, !(), !*World)
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

//TODO: can we implement errors cases as MaybeError instead of trace_n?
onNotification :: !NotificationMessage !() !*World -> (![!NotificationMessage], !(), !*World)
onNotification {NotificationMessage| method, params} st world =
	case method of
		"textDocument/didSave"
			| isNothing params = ([!], st, trace_n "Missing argument for 'textDocument/didSave'." world)
			# (diag, world) = diagnosticsFor (deserialize $ fromJust params) world
			= ([!notificationMessage "textDocument/publishDiagnostics" (?Just diag)], st, world)
		_
			= ([!], st, trace_n (concat3 "Unknown notification '" method "'.") world)

diagnosticsFor :: !DidSaveTextDocumentParams !*World -> (!?'LSP.PublishDiagnosticsParams'.PublishDiagnosticsParams, !*World)
diagnosticsFor params world
	# (diagnostics, world) = runCompiler params.textDocument.TextDocumentIdentifier.uri.uriPath world
	// TODO: error handling for `diagnostics`
	= (?Just
		{ 'LSP.PublishDiagnosticsParams'.uri = params.textDocument.TextDocumentIdentifier.uri
		, 'LSP.PublishDiagnosticsParams'.diagnostics = Map lspDiagnosticFor $ fromOk diagnostics
		}
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
