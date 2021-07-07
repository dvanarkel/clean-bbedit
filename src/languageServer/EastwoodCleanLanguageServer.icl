module EastwoodCleanLanguageServer

import StdEnv
import Data.Either
import LSP
import LSP.ResponseMessage
import LSP.RequestMessage
import LSP.ServerCapabilities

exampleLanguageServer :: LanguageServer ()
exampleLanguageServer =
	{ LanguageServer
	| initialState = ()
	, onRequest = onRequest
	// One must not reply to Notifications
	, onNotification = (\_ _ -> ())
	}

onRequest :: !RequestMessage !() -> (ResponseMessage, ())
onRequest {RequestMessage | id} _ = (errorResponse id, ())
where
	errorResponse :: !(Either Int String) -> ResponseMessage
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

myCapabilities :: ServerCapabilities
myCapabilities =
	{ ServerCapabilities
	| declarationProvider = ?None
	, definitionProvider = ?Just True
	}

Start :: !*World -> *World
Start w = serve myCapabilities exampleLanguageServer w
