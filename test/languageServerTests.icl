module languageServerTests

import StdEnv
import Data.Func, Data.Error
import Text, Text.GenPrint
import System.Directory
import System.FilePath
import System._Unsafe
from System.Process import :: ProcessIO {..}
from Gast import
	:: Property, :: Testoption (Bent), =.=, generic genShow, as, name, :: PrintOption (OutputTestEvents),
	instance Testable Property
import Gast.CommandLine
import Common

Start :: *World -> *World
Start world = exposeProperties [OutputTestEvents] [Bent] properties world

properties :: [Property]
properties =:
	[ initializesCorrectly as "language server initializes correctly"
	, didSaveNotificationCorrectlyHandledFor "ok" [!("ok.icl", "[]")] as
		"language server handles didSave notification correctly for program without issues"
	, didSaveNotificationCorrectlyHandledFor
		"errors"
		[!	( "errors.icl"
			, "[{\"range\":{\"start\":{\"line\":19,\"character\":0},\"end\":{\"line\":20,\"character\":0}},\"severity\":1,\"message\":\"Type error [errors.icl,20,h]: near list constructor : cannot unify demanded type with offered type:\\n Int\\n [[Int]]\\n\",\"tags\":[],\"relatedInformation\":[]},{\"range\":{\"start\":{\"line\":16,\"character\":0},\"end\":{\"line\":17,\"character\":0}},\"severity\":1,\"message\":\"Type error [errors.icl,17,g]: near list constructor : cannot unify demanded type with offered type:\\n Int\\n [Int]\",\"tags\":[],\"relatedInformation\":[]},{\"range\":{\"start\":{\"line\":11,\"character\":0},\"end\":{\"line\":12,\"character\":0}},\"severity\":1,\"message\":\"Type error [errors.icl,12,f]: near Unit : cannot unify demanded type with offered type:\\n Int\\n ()\",\"tags\":[],\"relatedInformation\":[]},{\"range\":{\"start\":{\"line\":7,\"character\":0},\"end\":{\"line\":8,\"character\":0}},\"severity\":1,\"message\":\"Type error [errors.icl,8,Start]: near Unit : cannot unify demanded type with offered type:\\n Int\\n ()\",\"tags\":[],\"relatedInformation\":[]},{\"range\":{\"start\":{\"line\":7,\"character\":0},\"end\":{\"line\":8,\"character\":0}},\"severity\":2,\"message\":\"Parse warning [errors.icl,8;11]: ! ignored\",\"tags\":[],\"relatedInformation\":[]}]"
			)
		]
		as
			"language server handles didSave notification correctly for program with various issues"
	, didSaveNotificationCorrectlyHandledFor
		"errorsInImportedDcl"
		[!	( "DclErrors.dcl"
			, "[{\"range\":{\"start\":{\"line\":2,\"character\":0},\"end\":{\"line\":3,\"character\":0}},\"severity\":1,\"message\":\"Error [DclErrors.dcl,3]: Unknown.dcl could not be imported\\n\",\"tags\":[],\"relatedInformation\":[]}]"
			)
		,	( "errorsInImportedDcl.icl"
			, "[]"
			)
		]
		as
			"language server handles didSave notification correctly for program importing a module with issues in the DCL"
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

didSaveNotificationCorrectlyHandledFor :: !String ![!(FilePath, String)] -> Property
didSaveNotificationCorrectlyHandledFor moduleName expectedDiags = accUnsafe didSaveNotificationCorrectlyHandled`
where
	didSaveNotificationCorrectlyHandled` :: !*World -> (Property, *World)
	didSaveNotificationCorrectlyHandled` world
	# (Ok curDir, world) = getCurrentDirectory world
	# testModulePath = testModulePathFor curDir (moduleName <.> "icl")
	# ((handle, io), world) = startLanguageServer world
	# world = writeMessage (generateMessage initializeRequestBody) io.stdIn world
	# (_, world) = readMessage io.stdOut world
	# world = writeMessage (generateMessage initializedNotificationBody) io.stdIn world // no response expected
	# world = writeMessage (generateMessage $ didSaveNotificationBodyFor testModulePath) io.stdIn world
	# (message, world) = readMessage io.stdOut world
	# world = shutdownLanguageServer handle world
	# expectedMessages =
		concat
			[ generateMessage $
				expectedDidSaveNotificationResponseBodyFor (testModulePathFor curDir expectedDiagFile) expectedDiags
			\\ (expectedDiagFile, expectedDiags) <|- expectedDiags
			]
	= (message =.= expectedMessages, world)

	testModulePathFor :: !FilePath !FilePath -> FilePath
	testModulePathFor dir moduleFile = dir </> "testPrograms" </> moduleFile

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

didSaveNotificationBodyFor :: !String -> String
didSaveNotificationBodyFor file =
	concat3
		"{\"method\":\"textDocument/didSave\",\"jsonrpc\":\"2.0\",\"params\":{\"textDocument\":{\"uri\":\"file://"
		file
		"\"}}}"

expectedDidSaveNotificationResponseBodyFor :: !String !String -> String
expectedDidSaveNotificationResponseBodyFor file expectedDiagnostics =
	concat5
		"{\"jsonrpc\":2.0,\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":\"file://"
		file
		"\",\"diagnostics\":"
		expectedDiagnostics
		"}}"
incorrectNotificationBody = "{\"method\":\"unknown/method\",\"jsonrpc\":\"2.0\",\"params\":{}}"
expectedErrorLogMessage = "{\"jsonrpc\":2.0,\"method\":\"window/logMessage\",\"params\":{\"type\":1,\"message\":\"Unknown notification \'unknown/method\'.\"}}"
