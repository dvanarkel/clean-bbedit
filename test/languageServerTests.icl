module languageServerTests

import StdEnv
import System._Unsafe
from System.Process import :: ProcessIO {..}
import Gast
import Gast.CommandLine
import Common

Start :: *World -> *World
Start world = exposeProperties [OutputTestEvents] [Bent] properties world

properties :: [Property]
properties =:
	[ initializesCorrectly as "language server initializes correctly"
	, didSaveNotificationCorrectlyHandled as "language server handles didSave notification correctly"
	, incorrectNotificationsResultsInErrorLog as "language server responds to unknown method with logMessage"
	]

initializesCorrectly :: Property
initializesCorrectly = accUnsafe initializesCorrectly`
where
	initializesCorrectly` :: !*World -> (Property, *World)
	initializesCorrectly` world
	# ((handle, io), world) = startLanguageServer world
	# world = writeMessage (generateMessage initializeRequestBody) io.stdIn world
	# (message, world) = readMessage io.stdOut world
	# world = shutdownLanguageServer handle world
	= (message =.= generateMessage expectedInitializeResponseBody, world)

didSaveNotificationCorrectlyHandled :: Property
didSaveNotificationCorrectlyHandled = accUnsafe didSaveNotificationCorrectlyHandled`
where
	didSaveNotificationCorrectlyHandled` :: !*World -> (Property, *World)
	didSaveNotificationCorrectlyHandled` world
	# ((handle, io), world) = startLanguageServer world
	# world = writeMessage (generateMessage initializeRequestBody) io.stdIn world
	# (_, world) = readMessage io.stdOut world
	# world = writeMessage (generateMessage initializedNotificationBody) io.stdIn world // no response expected
	# world = writeMessage (generateMessage didSaveNotificationBody) io.stdIn world
	# (message, world) = readMessage io.stdOut world
	# world = shutdownLanguageServer handle world
	= (message =.= generateMessage expectedDidSaveNotificationResponseBody, world)

incorrectNotificationsResultsInErrorLog :: Property
incorrectNotificationsResultsInErrorLog = accUnsafe incorrectNotificationsResultsInErrorLog`
where
	incorrectNotificationsResultsInErrorLog` :: !*World -> (Property, *World)
	incorrectNotificationsResultsInErrorLog` world
	# ((handle, io), world) = startLanguageServer world
	# world = writeMessage (generateMessage initializeRequestBody) io.stdIn world
	# (_, world) = readMessage io.stdOut world
	# world = writeMessage (generateMessage initializedNotificationBody) io.stdIn world // no response expected
	# world = writeMessage (generateMessage incorrectNotificationBody) io.stdIn world
	# (message, world) = readMessage io.stdOut world
	# world = shutdownLanguageServer handle world
	= (message =.= generateMessage expectedErrorLogMessage, world)


initializeRequestBody = "{\"id\": 1, \"jsonrpc\": \"2.0\", \"method\": \"initialize\", \"params\": {\"initializationOptions\": {}, \"rootUri\": \"file:///home/erin/Projects/clean-lsp\", \"workspaceFolders\": [{\"uri\": \"file:///home/erin/Projects/clean-lsp\", \"name\": \"/home/erin/Projects/clean-lsp\"}], \"rootPath\": \"/home/erin/Projects/clean-lsp\", \"clientInfo\": {\"version\": \"0.6.0\", \"name\": \"Neovim\"}, \"processId\": 55832, \"trace\": \"off\", \"capabilities\": {\"callHierarchy\": {\"dynamicRegistration\": false}, \"window\": {\"showDocument\": {\"support\": false}, \"showMessage\": {\"messageActionItem\": {\"additionalPropertiesSupport\": false}}, \"workDoneProgress\": true}, \"workspace\": {\"workspaceFolders\": true, \"applyEdit\": true, \"workspaceEdit\": {\"resourceOperations\": [\"rename\", \"create\", \"delete\"]}, \"symbol\": {\"symbolKind\": {\"valueSet\": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]}, \"dynamicRegistration\": false, \"hierarchicalWorkspaceSymbolSupport\": true}, \"configuration\": true}, \"textDocument\": {\"documentSymbol\": {\"symbolKind\": {\"valueSet\": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]}, \"dynamicRegistration\": false, \"hierarchicalDocumentSymbolSupport\": true}, \"completion\": {\"completionItem\": {\"snippetSupport\": false, \"commitCharactersSupport\": false, \"preselectSupport\": false, \"deprecatedSupport\": false, \"documentationFormat\": [\"markdown\", \"plaintext\"]}, \"contextSupport\": false, \"dynamicRegistration\": false, \"completionItemKind\": {\"valueSet\": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]}}, \"publishDiagnostics\": {\"relatedInformation\": true, \"tagSupport\": {\"valueSet\": [1, 2]}}, \"rename\": {\"prepareSupport\": true, \"dynamicRegistration\": false}, \"definition\": {\"linkSupport\": true}, \"references\": {\"dynamicRegistration\": false}, \"codeAction\": {\"codeActionLiteralSupport\": {\"codeActionKind\": {\"valueSet\": [\"\", \"Empty\", \"QuickFix\", \"Refactor\", \"RefactorExtract\", \"RefactorInline\", \"RefactorRewrite\", \"Source\", \"SourceOrganizeImports\", \"quickfix\", \"refactor\", \"refactor.extract\", \"refactor.inline\", \"refactor.rewrite\", \"source\", \"source.organizeImports\"]}}, \"dynamicRegistration\": false}, \"declaration\": {\"linkSupport\": true}, \"signatureHelp\": {\"signatureInformation\": {\"documentationFormat\": [\"markdown\", \"plaintext\"]}, \"dynamicRegistration\": false}, \"documentHighlight\": {\"dynamicRegistration\": false}, \"hover\": {\"dynamicRegistration\": false, \"contentFormat\": [\"markdown\", \"plaintext\"]}, \"synchronization\": {\"didSave\": true, \"willSaveWaitUntil\": false, \"willSave\": false, \"dynamicRegistration\": false}, \"implementation\": {\"linkSupport\": true}, \"typeDefinition\": {\"linkSupport\": true}}}}}"
expectedInitializeResponseBody = "{\"jsonrpc\":2.0,\"id\":1,\"result\":{\"capabilities\":{\"textDocumentSync\":{\"save\":true}},\"serverInfo\":{\"name\":\"Eastwood\",\"version\":\"WIP\"}}}"
initializedNotificationBody = "{\"jsonrpc\": \"2.0\", \"method\": \"initialized\", \"params\": {}}"
didSaveNotificationBody = "{\"method\":\"textDocument/didSave\",\"jsonrpc\":\"2.0\",\"params\":{\"textDocument\":{\"uri\":\"file:///tmp/test.icl\"}}}"
expectedDidSaveNotificationResponseBody = "{\"jsonrpc\":2.0,\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":\"file:///tmp/test.icl\",\"diagnostics\":[]}}"
incorrectNotificationBody = "{\"method\":\"unknown/method\",\"jsonrpc\":\"2.0\",\"params\":{}}"
expectedErrorLogMessage = "{\"jsonrpc\":2.0,\"method\":\"window/logMessage\",\"params\":{\"type\":1,\"message\":\"Unknown notification \'unknown/method\'.\"}}"
