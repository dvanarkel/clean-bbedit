module EastwoodCleanLanguageServer

import StdEnv, StdDebug, StdOverloadedList
import Data.Either, Data.Func, Data.Maybe
from Data.Error import fromOk
import Text, Text.GenJSON, Text.URI
import LSP, LSP.ResponseMessage, LSP.RequestMessage, LSP.NotificationMessage, LSP.ServerCapabilities
import LSP.DidSaveTextDocumentParams, LSP.TextDocumentIdentifier, LSP.PublishDiagnosticsParams, LSP.Diagnostic
import LSP.Range, LSP.Position, LSP.BasicTypes
import LSP.Internal.Serialize
from Eastwood.Diagnostic import
	:: EastwoodDiagnostic {..}, :: DiagnosticSource, :: EastwoodDiagnosticSeverity
import qualified Eastwood.Diagnostic
from Eastwood.Range import
	:: EastwoodRange {..}, :: EastwoodPosition {..}, :: CharacterRange
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

diagnosticsFor :: !DidSaveTextDocumentParams !*World -> (!?PublishDiagnosticsParams, !*World)
diagnosticsFor {textDocument = {TextDocumentIdentifier| uri}} world
	# (diagnostics, world) = runCompiler uri.uriPath world
	// TODO: error handling for `diagnostics`
	= (?Just {uri = uri, diagnostics = Map lspDiagnosticFor $ fromOk diagnostics}, world)
where
	lspDiagnosticFor :: !EastwoodDiagnostic -> Diagnostic
	lspDiagnosticFor {EastwoodDiagnostic| range, message, severity}  =
		{ range = rangeCorrespondingTo range
		, severity = ?Just $ severityCorrespondingTo severity
		, codeDescription = ?None
		, source = ?None
		, message = message
		, tags = [!]
		, relatedInformation = [!]
		, data = JSONNull
		}

rangeCorrespondingTo :: !CharacterRange -> Range
rangeCorrespondingTo {EastwoodRange| start, end} =
	{Range| start = positionCorrespondingTo start, end = positionCorrespondingTo end}
where
	positionCorrespondingTo :: !EastwoodPosition -> Position
	positionCorrespondingTo {EastwoodPosition| line, character} =
		{Position| line = uint line, character = uint character}

severityCorrespondingTo :: !EastwoodDiagnosticSeverity -> DiagnosticSeverity
severityCorrespondingTo 'Eastwood.Diagnostic'.Error       = Error
severityCorrespondingTo 'Eastwood.Diagnostic'.Warning     = Warning
severityCorrespondingTo 'Eastwood.Diagnostic'.Hint        = Hint
severityCorrespondingTo 'Eastwood.Diagnostic'.Information = Information
