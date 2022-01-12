definition module GotoDefinition

from LSP.RequestMessage import :: RequestMessage
from LSP.ResponseMessage import :: ResponseMessage
from Config import :: EastwoodState

onGotoDefinition :: !RequestMessage !EastwoodState !*World -> (!ResponseMessage, !*World)
