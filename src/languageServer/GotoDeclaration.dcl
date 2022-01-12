definition module GotoDeclaration

from LSP.RequestMessage import :: RequestMessage
from LSP.ResponseMessage import :: ResponseMessage
from Config import :: EastwoodState

/**
 * Sends a response to the client containing the locations of the declarations which were requested by the client.
 * @param The request.
 * @param The Eastwood state.
 * @result The response.
 */
onGotoDeclaration
	:: !RequestMessage !EastwoodState !*World -> (!ResponseMessage, !*World)
