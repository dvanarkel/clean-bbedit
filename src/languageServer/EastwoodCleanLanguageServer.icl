module EastwoodCleanLanguageServer

import StdEnv
import StdOverloadedList

from Data.Error import fromOk
import qualified Data.Error
import Data.Func
import Data.Maybe
import Text
import Text.GenJSON
import Text.URI

from LSP.Diagnostic import qualified :: Diagnostic {..}, :: DiagnosticSeverity {..}
from LSP.Position import qualified :: Position {..}
from LSP.PublishDiagnosticsParams import qualified :: PublishDiagnosticsParams {..}
from LSP.Range import qualified :: Range {..}
import LSP
import LSP.BasicTypes
import LSP.DidSaveTextDocumentParams
import LSP.Internal.Serialize
import LSP.LogMessageParams
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

onNotification :: !NotificationMessage !() !*World -> (![!NotificationMessage], !(), !*World)
onNotification {NotificationMessage| method, params} st world =
	case method of
		"textDocument/didSave"
			| isNothing params
				= ([!errorLogMessage "Missing argument for 'textDocument/didSave'."] , st, world)
			# (diag, world) = diagnosticsFor (deserialize $ fromJust params) world
			= case diag of
				?None =
					([!], st, world)
				?Just ('Data.Error'.Ok diag) =
					([!notificationMessage "textDocument/publishDiagnostics" (?Just diag)], st, world)
				?Just ('Data.Error'.Error err) =
					([!errorLogMessage err], st, world)
		_
			= ([!errorLogMessage $ concat3 "Unknown notification '" method "'."], st, world)
where
	errorLogMessage :: !String -> NotificationMessage
	errorLogMessage message = logMessage
		{ LogMessageParams
		| type = Error
		, message = message
		}

diagnosticsFor ::
	!DidSaveTextDocumentParams !*World
	-> (!?(MaybeError String 'LSP.PublishDiagnosticsParams'.PublishDiagnosticsParams), !*World)
diagnosticsFor params world
	# (diagnostics, world) = runCompiler params.textDocument.TextDocumentIdentifier.uri.uriPath world
	= (?Just
		case diagnostics of
			'Data.Error'.Ok diagnostics =
				'Data.Error'.Ok { 'LSP.PublishDiagnosticsParams'.uri = params.textDocument.TextDocumentIdentifier.uri
				, 'LSP.PublishDiagnosticsParams'.diagnostics = Map lspDiagnosticFor diagnostics
				}
			error = 'Data.Error'.liftError error
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
