definition module GotoDeclaration

from LSP.RequestMessage import :: RequestMessage
from LSP.ResponseMessage import :: ResponseMessage
from Config import :: EastwoodState

/**
 * Sends a response to the client containing the locations of the declarations which were requested by the client.
 * based on a request.
 */
onGotoDeclaration
	:: !RequestMessage !EastwoodState !*World -> (!ResponseMessage, !EastwoodState, !*World)
