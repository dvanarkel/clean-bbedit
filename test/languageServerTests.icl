module languageServerTests

import StdEnv
import StdMaybe
import Data.Func, Data.Error
import Data.Maybe.Gast
import Data.Maybe.GenPrint
import System.Directory
from System.FilePath import </>
from System.Process import :: ProcessIO {..}, checkProcess
import System.Time
import System._Unsafe
import Text
from Gast import
	:: Property, :: Testoption (Bent), =.=, generic genShow, as, name, :: PrintOption (OutputTestEvents),
	instance Testable Property, instance Testable Bool,
	class /\(..), instance /\ Property Property
import Gast.CommandLine
import Common

SUITE_DEFAULT :== "suite-default"
SUITE_WITHOUT_CONFIG :== "suite-without-config"
SUITE_CONFIG_NON_EXISTING_PATHS :== "suite-config-non-existing-paths"
SUITE_CONFIG_MISSING_PATHS :== "suite-config-missing-paths"
SUITE_CONFIG_NO_PATHS_KEY :== "suite-config-no-paths-key"

// A non-existing file. Used when the actual file doesn't matter, because Eastwood is expected to run into a more
// fundamental edge-case.
FILE_DONT_CARE :== "dontcare.icl"

Start :: *World -> *World
Start world = exposeProperties [OutputTestEvents] [Bent] properties world

properties :: [Property]
properties =:
	[ initializesCorrectly SUITE_DEFAULT as "language server initializes correctly"
	, setTraceIgnored SUITE_DEFAULT as "$/setTrace notification is ignored"
	, didSaveNotificationCorrectlyHandledFor SUITE_DEFAULT "ok" [!("ok.icl", "[]")] as
		"language server handles didSave notification correctly for program without issues"
	, didSaveNotificationCorrectlyHandledFor
		SUITE_DEFAULT
		"errors"
		[!("errors.icl", diagnosticsForErrors)
		]
		as
			"language server handles didSave notification correctly for program with various issues"
	, didSaveNotificationCorrectlyHandledFor
		SUITE_DEFAULT
		"errorsInImportedDcl"
		[!("DclErrors.dcl", diagnosticsForDclErrors)
		, ("errorsInImportedDcl.icl", noDiagnostics)
		]
		as
			"language server handles didSave notification correctly for program importing a module with issues in the DCL"
	, incorrectNotificationsResultsInErrorLog SUITE_DEFAULT as "language server responds to unknown method with showMessage"
	, compilerRuntimeErrorHandled SUITE_DEFAULT "TooLarge.icl" as "compiler runtime errors are handled"
	, configMissingValueForPaths SUITE_CONFIG_MISSING_PATHS FILE_DONT_CARE as "Error notification is shown when there is no value for paths field."
	, configPathsSectionMissing SUITE_CONFIG_NO_PATHS_KEY FILE_DONT_CARE as "Error notification is shown when paths section is missing in config."
	, configIsMissingResultsInErrorLogOnSave SUITE_WITHOUT_CONFIG FILE_DONT_CARE
		as "Error notification is shown when config is missing and a module is saved."
	, configHasNonExistingPathsResultsInErrorLogOnSave SUITE_CONFIG_NON_EXISTING_PATHS FILE_DONT_CARE
		as "Error notification is shown when config contains non-existing paths and a module is saved."
	, didSaveNotificationCorrectlyHandledFor SUITE_DEFAULT
		"nonexisting"
		[!("nonexisting.icl", diagnosticsForNonexisting)]
		as "notifications for non-existing modules yield an error"
	, didSaveNotificationCorrectlyHandledFor SUITE_DEFAULT
		("someLib" </> "TestModule")
		[!("someLib" </> "TestModule.icl", noDiagnostics)]
		as "hierarchical modules are correctly compiled"
	, didSaveNotificationCorrectlyHandledFor SUITE_DEFAULT
		("someLib" </> "MainModule")
		[!("DclErrors.dcl", diagnosticsForDclErrors)
		, ("someLib" </> "MainModule.icl", noDiagnostics)
		]
		as "diagnostics from files in other directories have the correct path"
	, didSaveNotificationCorrectlyHandledFor SUITE_DEFAULT
		"WrongModuleName"
		[!("WrongModuleName.icl", diagnosticsForWrongModuleName)]
		as "correct notifications for files with the wrong module name"
	, didSaveNotificationCorrectlyHandledFor SUITE_DEFAULT
		("otherLib" </> "IncorrectModuleHeader")
		[!("otherLib" </> "IncorrectModuleHeader.icl", diagnosticsForIncorrectModuleHeader)]
		as "correct notifications for file with the wrong module name, depending on the search paths"
	, didCloseIgnored SUITE_DEFAULT "ok.icl" as "didClose notification is ignored"
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

initializesCorrectly :: !String -> Property
initializesCorrectly suite = accUnsafe initializesCorrectly`
where
	initializesCorrectly` :: !*World -> (Property, *World)
	initializesCorrectly` world
	# ((handle, io), world) = startLanguageServer world
	# (Ok currentDirectory, world) = getCurrentDirectory world
	# world =
		writeMessage (generateMessage $ initializeRequestBody $ currentDirectory </> suite) io.stdIn world
	# (message, world) = readMessage io.stdOut world
	# (finalOut, world) = shutdownLanguageServer handle io world
	= (message =.= generateMessage expectedInitializeResponseBody /\ finalOut =.= ?None, world)

setTraceIgnored :: !String -> Property
setTraceIgnored suite = accUnsafe setTraceIgnored`
where
	setTraceIgnored` :: !*World -> (Property, *World)
	setTraceIgnored` world
	# (Ok curDir, world) = getCurrentDirectory world
	# ((handle, io), world) = startLanguageServer world
	# world = writeMessage (generateMessage $ initializeRequestBody $ curDir </> suite) io.stdIn world
	# (_, world) = readMessage io.stdOut world
	# world = writeMessage (generateMessage initializedNotificationBody) io.stdIn world // no response expected
	# world = writeMessage (generateMessage setTraceNotificationBody) io.stdIn world // no response expected
	# (finalOut, world) = shutdownLanguageServer handle io world
	= (finalOut =.= ?None, world)

configMissingValueForPaths :: !String !String -> Property
configMissingValueForPaths suite file = accUnsafe configMissingValueForPaths`
where
	configMissingValueForPaths` :: !*World -> (!Property, !*World)
	configMissingValueForPaths` world
		# (Ok curDir, world) = getCurrentDirectory world
		= assertResponseForSaveNotification
			suite
			file
			(expectedDiagnosticsResponseBody curDir)
			world

	expectedDiagnosticsResponseBody :: !String -> String
	expectedDiagnosticsResponseBody curDir =
		concat3
			"{\"jsonrpc\":2.0,\"method\":\"window/showMessage\",\"params\":{\"type\":1,\"message\":\"Invalid format of project file "
			(curDir </> suite </> "Eastwood.yml")
			": Error occurred while constructing YAML: invalid content: expected sequence for list.The following hints were provided for solving the error: Error occurred while parsing record \\\"CompilerSettingsConfig\\\". Error occurred while parsing field \\\"paths\\\". The expected format of the project file is described in https://gitlab.com/top-software/eastwood/-/blob/main/README.md\"}}"

configPathsSectionMissing :: !String !String -> Property
configPathsSectionMissing suite file = accUnsafe configPathsSectionIsMissing`
where
	configPathsSectionIsMissing` :: !*World -> (!Property, !*World)
	configPathsSectionIsMissing` world
		# (Ok curDir, world) = getCurrentDirectory world
		= assertResponseForSaveNotification
			suite
			file
			(expectedDiagnosticsResponseBody curDir)
			world

	expectedDiagnosticsResponseBody :: !String -> String
	expectedDiagnosticsResponseBody curDir =
		concat3 "{\"jsonrpc\":2.0,\"method\":\"window/showMessage\",\"params\":{\"type\":1,\"message\":\"Invalid format of project file "
		(curDir </> suite </> "Eastwood.yml")
		": Error occurred while constructing YAML: invalid content: required key paths is not specified.The following hints were provided for solving the error: Error occurred while parsing record \\\"CompilerSettingsConfig\\\". The expected format of the project file is described in https://gitlab.com/top-software/eastwood/-/blob/main/README.md\"}}"


configIsMissingResultsInErrorLogOnSave :: !String !String -> Property
configIsMissingResultsInErrorLogOnSave suite file
	= accUnsafe $ assertResponseForSaveNotification suite file expectedDiagnosticsResponseBody
where
	expectedDiagnosticsResponseBody :: String
	expectedDiagnosticsResponseBody = "{\"jsonrpc\":2.0,\"method\":\"window/showMessage\",\"params\":{\"type\":1,\"message\":\"Could not find the Eastwood.yml project configuration file in the workspace folder. Please create the file in the workspace\'s root folder. The expected format of the Eastwood.yml file is described in https://gitlab.com/top-software/eastwood/-/blob/main/README.md.\"}}"

configHasNonExistingPathsResultsInErrorLogOnSave :: !String !String -> Property
configHasNonExistingPathsResultsInErrorLogOnSave suite file
	= accUnsafe $ configIsMissingResultsInErrorLogOnSave`
where
	configIsMissingResultsInErrorLogOnSave` :: !*World -> (!Property, !*World)
	configIsMissingResultsInErrorLogOnSave` world
		# (Ok curDir, world) = getCurrentDirectory world
		= assertResponseForSaveNotification
			suite
			file
			(expectedDiagnosticsResponseBody curDir)
			world
	expectedDiagnosticsResponseBody :: !String -> String
	expectedDiagnosticsResponseBody curDir =
		concat3
			"{\"jsonrpc\":2.0,\"method\":\"window/showMessage\",\"params\":{\"type\":1,\"message\":\"Failed to find full path of "
			(curDir </> suite </> "nonexisting")
			" mentioned in Eastwood.yml: No such file or directory\"}}"

assertResponseForSaveNotification :: !String !String !String !*World -> (!Property, !*World)
assertResponseForSaveNotification suite file expectedResponseBody world
	# (Ok curDir, world) = getCurrentDirectory world
	# testModulePath = curDir </> suite </> file
	# (response, finalOut, world) = singleMessageResponse suite (didSaveNotificationBodyFor testModulePath) world
	=	( name "didSave notification response is correct response" $ response =.= generateMessage expectedResponseBody
		, world)

didSaveNotificationCorrectlyHandledFor :: !String !String ![!(FilePath, String)] -> Property
didSaveNotificationCorrectlyHandledFor suite moduleName expectedDiags = accUnsafe didSaveNotificationCorrectlyHandled`
where
	didSaveNotificationCorrectlyHandled` :: !*World -> (Property, *World)
	didSaveNotificationCorrectlyHandled` world
	# (Ok curDir, world) = getCurrentDirectory world
	# testModulePath = curDir </> suite </> (moduleName +++ ".icl")
	# (message, finalOut, world)
		= singleMessageResponse suite (didSaveNotificationBodyFor testModulePath) world
	# expectedMessages =
		concat
			[ generateMessage $
				expectedDidSaveNotificationResponseBodyFor (curDir </> suite </> expectedDiagFile) expectedDiags
			\\ (expectedDiagFile, expectedDiags) <|- expectedDiags
			]
	= (message =.= expectedMessages /\ finalOut =.= ?None, world)

incorrectNotificationsResultsInErrorLog :: !String -> Property
incorrectNotificationsResultsInErrorLog suite = accUnsafe incorrectNotificationsResultsInErrorLog`
where
	incorrectNotificationsResultsInErrorLog` :: !*World -> (Property, *World)
	incorrectNotificationsResultsInErrorLog` world
	# (message, finalOut, world) = singleMessageResponse suite incorrectNotificationBody world
	= (message =.= generateMessage expectedErrorLogMessage /\ finalOut =.= ?None, world)

didCloseIgnored :: !String !String -> Property
didCloseIgnored suite file = accUnsafe didCloseIgnored`
where
	didCloseIgnored` :: !*World -> (Property, *World)
	didCloseIgnored` world
	# (Ok curDir, world) = getCurrentDirectory world
	# testModulePath = curDir </> suite </> file
	# ((handle, io), world) = startLanguageServer world
	# world = writeMessage (generateMessage $ initializeRequestBody $ curDir </> suite) io.stdIn world
	# (_, world) = readMessage io.stdOut world
	# world = writeMessage (generateMessage initializedNotificationBody) io.stdIn world // no response expected
	# world = writeMessage (generateMessage $ didCloseNotificationBodyFor testModulePath) io.stdIn world
	# (finalOut, world) = shutdownLanguageServer handle io world
	= (finalOut =.= ?None, world)

compilerRuntimeErrorHandled :: !String !String -> Property
compilerRuntimeErrorHandled suite file = accUnsafe compilerRuntimeErrorHandled`
where
	compilerRuntimeErrorHandled` :: !*World -> (Property, *World)
	compilerRuntimeErrorHandled` world
	# (Ok curDir, world) = getCurrentDirectory world
	# testModulePath = curDir </> suite </> file
	# (message, finalOut, world) = singleMessageResponse suite (didSaveNotificationBodyFor testModulePath) world
	= (message =.= generateMessage expected /\ finalOut =.= ?None, world)

	expected = "{\"jsonrpc\":2.0,\"method\":\"window/showMessage\",\"params\":{\"type\":1,\"message\":\"The compiler crashed with the output:\\nStack overflow.\\n\"}}"

singleMessageResponse :: !String !String !*World -> (String, ?String, *World)
singleMessageResponse suite message world
# ((handle, io), world) = startLanguageServer world
# (Ok currentDirectory, world) = getCurrentDirectory world
# world = writeMessage (generateMessage $ initializeRequestBody $ currentDirectory </> suite) io.stdIn world
# (_, world) = readMessage io.stdOut world
# world = writeMessage (generateMessage initializedNotificationBody) io.stdIn world // no response expected
# world = writeMessage (generateMessage message) io.stdIn world
// Sleep for 500ms to make sure all messages are received.
# (_, world) = timespecSleep {tv_sec=0, tv_nsec=500000000} world
# (message, world) = readMessage io.stdOut world
# (finalOut, world) = shutdownLanguageServer handle io world
= (message, finalOut, world)

initializeRequestBody currentPath = concat3
	"{\"id\": 1, \"jsonrpc\": \"2.0\", \"method\": \"initialize\", \"params\": {\"initializationOptions\": {}, \"rootPath\": \""
	currentPath
	"\", \"clientInfo\": {\"version\": \"0.6.0\", \"name\": \"Neovim\"}, \"processId\": 55832, \"trace\": \"off\", \"capabilities\": {\"callHierarchy\": {\"dynamicRegistration\": false}, \"window\": {\"showDocument\": {\"support\": false}, \"showMessage\": {\"messageActionItem\": {\"additionalPropertiesSupport\": false}}, \"workDoneProgress\": true}, \"workspace\": {\"workspaceFolders\": true, \"applyEdit\": true, \"workspaceEdit\": {\"resourceOperations\": [\"rename\", \"create\", \"delete\"]}, \"symbol\": {\"symbolKind\": {\"valueSet\": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]}, \"dynamicRegistration\": false, \"hierarchicalWorkspaceSymbolSupport\": true}, \"configuration\": true}, \"textDocument\": {\"documentSymbol\": {\"symbolKind\": {\"valueSet\": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]}, \"dynamicRegistration\": false, \"hierarchicalDocumentSymbolSupport\": true}, \"completion\": {\"completionItem\": {\"snippetSupport\": false, \"commitCharactersSupport\": false, \"preselectSupport\": false, \"deprecatedSupport\": false, \"documentationFormat\": [\"markdown\", \"plaintext\"]}, \"contextSupport\": false, \"dynamicRegistration\": false, \"completionItemKind\": {\"valueSet\": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]}}, \"publishDiagnostics\": {\"relatedInformation\": true, \"tagSupport\": {\"valueSet\": [1, 2]}}, \"rename\": {\"prepareSupport\": true, \"dynamicRegistration\": false}, \"definition\": {\"linkSupport\": true}, \"references\": {\"dynamicRegistration\": false}, \"codeAction\": {\"codeActionLiteralSupport\": {\"codeActionKind\": {\"valueSet\": [\"\", \"Empty\", \"QuickFix\", \"Refactor\", \"RefactorExtract\", \"RefactorInline\", \"RefactorRewrite\", \"Source\", \"SourceOrganizeImports\", \"quickfix\", \"refactor\", \"refactor.extract\", \"refactor.inline\", \"refactor.rewrite\", \"source\", \"source.organizeImports\"]}}, \"dynamicRegistration\": false}, \"declaration\": {\"linkSupport\": true}, \"signatureHelp\": {\"signatureInformation\": {\"documentationFormat\": [\"markdown\", \"plaintext\"]}, \"dynamicRegistration\": false}, \"documentHighlight\": {\"dynamicRegistration\": false}, \"hover\": {\"dynamicRegistration\": false, \"contentFormat\": [\"markdown\", \"plaintext\"]}, \"synchronization\": {\"didSave\": true, \"willSaveWaitUntil\": false, \"willSave\": false, \"dynamicRegistration\": false}, \"implementation\": {\"linkSupport\": true}, \"typeDefinition\": {\"linkSupport\": true}}}}}"

expectedInitializeResponseBody = "{\"jsonrpc\":2.0,\"id\":1,\"result\":{\"capabilities\":{\"textDocumentSync\":{\"openClose\":true,\"save\":true}},\"serverInfo\":{\"name\":\"Eastwood\",\"version\":\"WIP\"}}}"
initializedNotificationBody = "{\"jsonrpc\": \"2.0\", \"method\": \"initialized\", \"params\": {}}"

setTraceNotificationBody = "{\"method\":\"$/setTrace\",\"jsonrpc\":\"2.0\",\"params\":{\"value\":\"off\"}}"

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

didCloseNotificationBodyFor :: !String -> String
didCloseNotificationBodyFor file =
	concat3
		"{\"method\":\"textDocument/didClose\",\"jsonrpc\":\"2.0\",\"params\":{\"textDocument\":{\"uri\":\"file://"
		file
		"\"}}}"
