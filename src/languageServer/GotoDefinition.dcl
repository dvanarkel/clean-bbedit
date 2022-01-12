definition module GotoDefinition

from LSP.RequestMessage import :: RequestMessage
from LSP.ResponseMessage import :: ResponseMessage
from Config import :: EastwoodState

/**
 * Sends a response to the client containing the locations of the definitions which were requested by the client.
 * @param The request.
 * @param The Eastwood state.
 * @result The response.
 */
onGotoDefinition :: !RequestMessage !EastwoodState !*World -> (!ResponseMessage, !*World)
