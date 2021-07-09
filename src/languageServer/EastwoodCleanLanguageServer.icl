module EastwoodCleanLanguageServer

import StdEnv
import Data.Either, Data.Func, Data.Maybe
import Text, Text.GenJSON
import LSP, LSP.ResponseMessage, LSP.RequestMessage, LSP.NotificationMessage, LSP.ServerCapabilities
import LSP.DidSaveTextDocumentParams, LSP.TextDocumentIdentifier, LSP.PublishDiagnosticsParams, LSP.Diagnostic
import LSP.Range, LSP.Position, LSP.BasicTypes
import LSP.Internal.Serialize

Start :: !*World -> *World
Start w = serve capabilities cleanLanguageServer w

capabilities :: ServerCapabilities
capabilities =
	{ ServerCapabilities
	| textDocumentSync = {save = True}
	}

cleanLanguageServer :: LanguageServer ()
cleanLanguageServer = {initialState = (), onRequest = onRequest, onNotification = onNotification}
import StdDebug
onRequest :: !RequestMessage !() -> (!ResponseMessage, !())
onRequest {RequestMessage | id} st = (errorResponse id, st)
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

onNotification :: !NotificationMessage !() -> (![!NotificationMessage], !())
onNotification {NotificationMessage| method, params} st =
	case method of
		"textDocument/didSave"
			| isNothing params = ([!], trace_n "Missing argument for 'textDocument/didSave'." st)
			=	(	[! notificationMessage
						"textDocument/publishDiagnostics" (?Just $ diagnosticsFor $ deserialize $ fromJust params)
					]
				, st
				)
		_
			= ([!], trace_n (concat3 "Unknown notification '" method "'.") st)

diagnosticsFor :: !DidSaveTextDocumentParams -> PublishDiagnosticsParams
diagnosticsFor {textDocument = {TextDocumentIdentifier| uri}} = {uri = uri, diagnostics = [!diagnostic]}
where
	diagnostic =
		{ range = {start = {line = uint 2, character = uint 0}, end = {line = uint 2, character = uint 10}}
		, severity = ?None
		, codeDescription = ?None
		, source = ?None
		, message = "test error message"
		, tags = [!]
		, relatedInformation = [!]
		, data = JSONNull
		}
