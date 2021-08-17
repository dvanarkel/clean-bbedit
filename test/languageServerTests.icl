module languageServerTests

import StdEnv
import StdMaybe
import Data.Func, Data.Error
import Data.Maybe.Gast
import Data.Maybe.GenPrint
import System.Time
import System.Directory
from System.FilePath import </>
import System._Unsafe
from System.Process import :: ProcessIO {..}, checkProcess
import Text
import Text.GenPrint
import Gast
import Gast.CommandLine
import Common

Start :: *World -> *World
Start world = exposeProperties [OutputTestEvents] [Bent] properties world

properties :: [Property]
properties =:
	[ initializesCorrectly as "language server initializes correctly"
	, failsToInitializeWithoutConfig as "refuses to initialize without Eastwood.yml"
	, didSaveNotificationCorrectlyHandledFor "ok" [!("ok.icl", "[]")] as
		"language server handles didSave notification correctly for program without issues"
	, didSaveNotificationCorrectlyHandledFor
		"errors"
		[!("errors.icl", diagnosticsForErrors)
		]
		as
			"language server handles didSave notification correctly for program with various issues"
	, didSaveNotificationCorrectlyHandledFor
		"errorsInImportedDcl"
		[!("DclErrors.dcl", diagnosticsForDclErrors)
		, ("errorsInImportedDcl.icl", noDiagnostics)
		]
		as
			"language server handles didSave notification correctly for program importing a module with issues in the DCL"
	, incorrectNotificationsResultsInErrorLog as "language server responds to unknown method with showMessage"
	, didSaveNotificationCorrectlyHandledFor
		"nonexisting"
		[!("nonexisting.icl", diagnosticsForNonexisting)]
		as "notifications for non-existing modules yield an error"
	, didSaveNotificationCorrectlyHandledFor
		("someLib" </> "TestModule")
		[!("someLib" </> "TestModule.icl", noDiagnostics)]
		as "hierarchical modules are correctly compiled"
	, didSaveNotificationCorrectlyHandledFor
		("someLib" </> "MainModule")
		[!("DclErrors.dcl", diagnosticsForDclErrors)
		, ("someLib" </> "MainModule.icl", noDiagnostics)
		]
		as "diagnostics from files in other directories have the correct path"
	, didSaveNotificationCorrectlyHandledFor
		"WrongModuleName"
		[!("WrongModuleName.icl", diagnosticsForWrongModuleName)]
		as "correct notifications for files with the wrong module name"
	, didSaveNotificationCorrectlyHandledFor
		("otherLib" </> "IncorrectModuleHeader")
		[!("otherLib" </> "IncorrectModuleHeader.icl", diagnosticsForIncorrectModuleHeader)]
		as "correct notifications for file with the wrong module name, depending on the search paths"
	]
where
	diagnosticsForErrors =
		"[{\"range\":{\"start\":{\"line\":19,\"character\":0},\"end\":{\"line\":19,\"character\":999999}},\"severity\":1,\"message\":\"Type error [errors.icl,20,h]: near list constructor : cannot unify demanded type with offered type:\\n Int\\n [[Int]]\\n\",\"tags\":[],\"relatedInformation\":[]},{\"range\":{\"start\":{\"line\":16,\"character\":0},\"end\":{\"line\":16,\"character\":999999}},\"severity\":1,\"message\":\"Type error [errors.icl,17,g]: near list constructor : cannot unify demanded type with offered type:\\n Int\\n [Int]\",\"tags\":[],\"relatedInformation\":[]},{\"range\":{\"start\":{\"line\":11,\"character\":0},\"end\":{\"line\":11,\"character\":999999}},\"severity\":1,\"message\":\"Type error [errors.icl,12,f]: near Unit : cannot unify demanded type with offered type:\\n Int\\n ()\",\"tags\":[],\"relatedInformation\":[]},{\"range\":{\"start\":{\"line\":7,\"character\":0},\"end\":{\"line\":7,\"character\":999999}},\"severity\":1,\"message\":\"Type error [errors.icl,8,Start]: near Unit : cannot unify demanded type with offered type:\\n Int\\n ()\",\"tags\":[],\"relatedInformation\":[]},{\"range\":{\"start\":{\"line\":7,\"character\":0},\"end\":{\"line\":7,\"character\":999999}},\"severity\":2,\"message\":\"Parse warning [errors.icl,8;11]: ! ignored\",\"tags\":[],\"relatedInformation\":[]}]"
	diagnosticsForDclErrors =
		"[{\"range\":{\"start\":{\"line\":2,\"character\":0},\"end\":{\"line\":2,\"character\":999999}},\"severity\":1,\"message\":\"Error [DclErrors.dcl,3]: Unknown.dcl could not be imported\\n\",\"tags\":[],\"relatedInformation\":[]}]"
	diagnosticsForNonexisting =
		"[{\"range\":{\"start\":{\"line\":0,\"character\":0},\"end\":{\"line\":0,\"character\":999999}},\"severity\":1,\"message\":\"Failed to determine module name: Cannot open\",\"tags\":[],\"relatedInformation\":[]}]"
	diagnosticsForWrongModuleName =
		"[{\"range\":{\"start\":{\"line\":0,\"character\":0},\"end\":{\"line\":0,\"character\":999999}},\"severity\":1,\"message\":\"Failed to determine module name: unexpected module name \'ThisIsNotTheRightModuleName\' in WrongModuleName\",\"tags\":[],\"relatedInformation\":[]}]"
	diagnosticsForIncorrectModuleHeader =
		"[{\"range\":{\"start\":{\"line\":0,\"character\":0},\"end\":{\"line\":0,\"character\":999999}},\"severity\":1,\"message\":\"Failed to determine module name: incomplete module name or missing search path\",\"tags\":[],\"relatedInformation\":[]}]"
	noDiagnostics =
		"[]"

initializesCorrectly :: Property
initializesCorrectly = accUnsafe initializesCorrectly`
where
	initializesCorrectly` :: !*World -> (Property, *World)
	initializesCorrectly` world
	# ((handle, io), world) = startLanguageServer world
	# (Ok currentDirectory, world) = getCurrentDirectory world
	# world = writeMessage (generateMessage $ initializeRequestBody $ currentDirectory </> "suite-001") io.stdIn world
	# (message, world) = readMessage io.stdOut world
	# world = shutdownLanguageServer handle io world
	= (message =.= generateMessage expectedInitializeResponseBody, world)

failsToInitializeWithoutConfig :: Property
failsToInitializeWithoutConfig = accUnsafe failsToInitializeWithoutConfig`
where
	failsToInitializeWithoutConfig` :: !*World -> (Property, *World)
	failsToInitializeWithoutConfig` world
	# ((handle, io), world) = startLanguageServer world
	# (Ok currentDirectory, world) = getCurrentDirectory world
	# world = writeMessage (generateMessage $ initializeRequestBody $ currentDirectory </> "suite-002") io.stdIn world
	# (message, world) = readMessage io.stdOut world
	// Wait a reasonable amount of time for the server to exit
	# (_, world) = timespecSleep {tv_sec=1, tv_nsec=0} world
	# (mbExitCode, world) = checkProcess handle world
	| isError mbExitCode = (name "failed to check process exit code" False, world)
	=	(	name "correct response" (message =.= generateMessage expectedInitializeResponseBody) /\
			name "language sever has exited" (fromOk mbExitCode =.= ?Just 0)
		, world
		)
	where
		expectedInitializeResponseBody =
			"{\"jsonrpc\":2.0,\"id\":1,\"error\":{\"code\":-32001,\"message\":\"Could not find Eastwood.yml\"}}"

didSaveNotificationCorrectlyHandledFor :: !String ![!(FilePath, String)] -> Property
didSaveNotificationCorrectlyHandledFor moduleName expectedDiags = accUnsafe didSaveNotificationCorrectlyHandled`
where
	didSaveNotificationCorrectlyHandled` :: !*World -> (Property, *World)
	didSaveNotificationCorrectlyHandled` world
	# (Ok curDir, world) = getCurrentDirectory world
	# testModulePath = testModulePathFor 1 curDir (moduleName +++ ".icl")
	# (message, world) = singleMessageResponse (didSaveNotificationBodyFor testModulePath) world
	# expectedMessages =
		concat
			[ generateMessage $
				expectedDidSaveNotificationResponseBodyFor (testModulePathFor 1 curDir expectedDiagFile) expectedDiags
			\\ (expectedDiagFile, expectedDiags) <|- expectedDiags
			]
	= (message =.= expectedMessages, world)

incorrectNotificationsResultsInErrorLog :: Property
incorrectNotificationsResultsInErrorLog = accUnsafe incorrectNotificationsResultsInErrorLog`
where
	incorrectNotificationsResultsInErrorLog` :: !*World -> (Property, *World)
	incorrectNotificationsResultsInErrorLog` world
	# (message, world) = singleMessageResponse incorrectNotificationBody world
	= (message =.= generateMessage expectedErrorLogMessage, world)

singleMessageResponse :: !String !*World -> (String, *World)
singleMessageResponse message world
# ((handle, io), world) = startLanguageServer world
# (Ok currentDirectory, world) = getCurrentDirectory world
# world = writeMessage (generateMessage $ initializeRequestBody $ currentDirectory </> "suite-001") io.stdIn world
# (_, world) = readMessage io.stdOut world
# world = writeMessage (generateMessage initializedNotificationBody) io.stdIn world // no response expected
# world = writeMessage (generateMessage message) io.stdIn world
# (message, world) = readMessage io.stdOut world
# world = shutdownLanguageServer handle io world
= (message, world)

initializeRequestBody currentPath = concat3
	"{\"id\": 1, \"jsonrpc\": \"2.0\", \"method\": \"initialize\", \"params\": {\"initializationOptions\": {}, \"rootPath\": \""
	currentPath
	"\", \"clientInfo\": {\"version\": \"0.6.0\", \"name\": \"Neovim\"}, \"processId\": 55832, \"trace\": \"off\", \"capabilities\": {\"callHierarchy\": {\"dynamicRegistration\": false}, \"window\": {\"showDocument\": {\"support\": false}, \"showMessage\": {\"messageActionItem\": {\"additionalPropertiesSupport\": false}}, \"workDoneProgress\": true}, \"workspace\": {\"workspaceFolders\": true, \"applyEdit\": true, \"workspaceEdit\": {\"resourceOperations\": [\"rename\", \"create\", \"delete\"]}, \"symbol\": {\"symbolKind\": {\"valueSet\": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]}, \"dynamicRegistration\": false, \"hierarchicalWorkspaceSymbolSupport\": true}, \"configuration\": true}, \"textDocument\": {\"documentSymbol\": {\"symbolKind\": {\"valueSet\": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]}, \"dynamicRegistration\": false, \"hierarchicalDocumentSymbolSupport\": true}, \"completion\": {\"completionItem\": {\"snippetSupport\": false, \"commitCharactersSupport\": false, \"preselectSupport\": false, \"deprecatedSupport\": false, \"documentationFormat\": [\"markdown\", \"plaintext\"]}, \"contextSupport\": false, \"dynamicRegistration\": false, \"completionItemKind\": {\"valueSet\": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]}}, \"publishDiagnostics\": {\"relatedInformation\": true, \"tagSupport\": {\"valueSet\": [1, 2]}}, \"rename\": {\"prepareSupport\": true, \"dynamicRegistration\": false}, \"definition\": {\"linkSupport\": true}, \"references\": {\"dynamicRegistration\": false}, \"codeAction\": {\"codeActionLiteralSupport\": {\"codeActionKind\": {\"valueSet\": [\"\", \"Empty\", \"QuickFix\", \"Refactor\", \"RefactorExtract\", \"RefactorInline\", \"RefactorRewrite\", \"Source\", \"SourceOrganizeImports\", \"quickfix\", \"refactor\", \"refactor.extract\", \"refactor.inline\", \"refactor.rewrite\", \"source\", \"source.organizeImports\"]}}, \"dynamicRegistration\": false}, \"declaration\": {\"linkSupport\": true}, \"signatureHelp\": {\"signatureInformation\": {\"documentationFormat\": [\"markdown\", \"plaintext\"]}, \"dynamicRegistration\": false}, \"documentHighlight\": {\"dynamicRegistration\": false}, \"hover\": {\"dynamicRegistration\": false, \"contentFormat\": [\"markdown\", \"plaintext\"]}, \"synchronization\": {\"didSave\": true, \"willSaveWaitUntil\": false, \"willSave\": false, \"dynamicRegistration\": false}, \"implementation\": {\"linkSupport\": true}, \"typeDefinition\": {\"linkSupport\": true}}}}}"

expectedInitializeResponseBody = "{\"jsonrpc\":2.0,\"id\":1,\"result\":{\"capabilities\":{\"textDocumentSync\":{\"openClose\":true,\"save\":true}},\"serverInfo\":{\"name\":\"Eastwood\",\"version\":\"WIP\"}}}"
initializedNotificationBody = "{\"jsonrpc\": \"2.0\", \"method\": \"initialized\", \"params\": {}}"

testModulePathFor :: !Int !FilePath !FilePath -> FilePath
testModulePathFor id dir moduleFile = dir </> "suite-" +++ lpad (toString id) 3 '0' </> moduleFile

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
expectedErrorLogMessage = "{\"jsonrpc\":2.0,\"method\":\"window/showMessage\",\"params\":{\"type\":1,\"message\":\"Unknown notification \'unknown/method\'.\"}}"
