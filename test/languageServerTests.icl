module languageServerTests

import StdEnv
import StdOverloadedList
import StdMaybe
import Data.Func, Data.Error
import Data.Tuple
import Data.Either
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

import LSP.RequestId
import LSP.BasicTypes
import LSP.Location
import LSP.ResponseMessage
import LSP.Internal.Serialize
import LSP.Position
import LSP.Range
import Text.GenJSON
import Text.URI

SUITE_DEFAULT :== "suite-default"
SUITE_WITHOUT_CONFIG :== "suite-without-config"
SUITE_CONFIG_NON_EXISTING_PATHS :== "suite-config-non-existing-paths"
SUITE_CONFIG_MISSING_PATHS :== "suite-config-missing-paths"
SUITE_CONFIG_NO_PATHS_KEY :== "suite-config-no-paths-key"
SUITE_CONFIG_EMPTY_PATHS :== "suite-config-empty-paths"

// A non-existing file. Used when the actual file doesn't matter, because Eastwood is expected to run into a more
// fundamental edge-case.
FILE_DONT_CARE :== "dontcare.icl"
FILE_OK :== "ok.icl"
FILE_GO_TO_DECLARATION_DCL_1 :== "GoToDeclarationModule1.dcl"
FILE_GO_TO_DECLARATION_ICL_1 :== "GoToDeclarationModule1.icl"
FILE_GO_TO_DECLARATION_DCL_2 :== "GoToDeclarationModule2.dcl"

Start :: *World -> *World
Start world = exposeProperties [OutputTestEvents] [Bent] properties world

properties :: [Property]
properties =:
	[ initializesCorrectly SUITE_DEFAULT as "language server initializes correctly"
	, setTraceIgnored SUITE_DEFAULT as "$/setTrace notification is ignored"
	, didSaveNotificationCorrectlyHandledFor SUITE_DEFAULT "ok" [!(FILE_OK, "[]")] as
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
	, compilerRuntimeErrorHandled SUITE_DEFAULT "tooLarge.icl" as "compiler runtime errors are handled"
	, configMissingValueForPaths SUITE_CONFIG_MISSING_PATHS FILE_DONT_CARE as "Error notification is shown when there is no value for paths field."
	, configPathsSectionMissing SUITE_CONFIG_NO_PATHS_KEY FILE_DONT_CARE as "Error notification is shown when paths section is missing in config."
	, configIsMissingResultsInErrorLogOnSave SUITE_WITHOUT_CONFIG FILE_DONT_CARE
		as "Error notification is shown when config is missing and a module is saved."
	, configHasNonExistingPathsResultsInErrorLogOnSave SUITE_CONFIG_NON_EXISTING_PATHS FILE_DONT_CARE
		as "Error notification is shown when config contains non-existing paths and a module is saved."
	, configPathEmpty SUITE_CONFIG_EMPTY_PATHS FILE_OK as "Empty path config still includes the root directory."
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
	, didCloseIgnored SUITE_DEFAULT FILE_OK as "didClose notification is ignored"
	, goToDeclarationOfTypeSingleResultIsCorrectlyHandledFor
		as "go to declaration of a type that is only declared in one module is correctly handled"
	, goToDeclarationOfTypeMultipleResultsIsCorrectlyHandledFor
		as "go to declaration of a type that is declared in two modules is correctly handled"
	, goToDeclarationOfFuncSingleResultIsCorrectlyHandledFor
		as "go to declaration of a function that is only declared in one module is correctly handled"
	, goToDeclarationOfFuncMultipleResultsIsCorrectlyHandledFor
		as "go to declaration of a function that is declared in two modules is correctly handled"
	, goToDeclarationOfInfixlFuncSingleResultIsCorrectlyHandledFor
		as "go to declaration of a infixl function that is only declared in one module is correctly handled"
	, goToDeclarationOfInfixrFuncSingleResultIsCorrectlyHandledFor
		as "go to declaration of a infixr function is correctly handled."
	, goToDeclarationOfInfixFuncSingleResultIsCorrectlyHandledFor
		as "go to declaration of a infix function is correctly handled."
	, goToDeclarationOfInfixlFuncMultipleResultsIsCorrectlyHandledFor
		as "go to declaration of a infixl function that is declared in two modules is correctly handled"
	, goToDeclarationOfInfixlFuncUsedPrefixSingleResultIsCorrectlyHandledFor
		as "go to declaration of a infixl function used prefix that is only declared in one module is correctly handled"
	, goToDeclarationOfInfixlFuncUsedPrefixMultipleResultsIsCorrectlyHandledFor
		as "go to declaration of a infixl function used prefix that is declared in two modules is correctly handled"
	, goToDeclarationOfFuncThatStartsWithUppercaseIsCorrectlyHandledFor
		as "go to declaration of a function whose name starts with an uppercase letter is correctly handled"
	, goToDeclarationOfDeriveGenericFuncSingleResultIsCorrectlyHandledFor
		as "go to declaration of a generic function that is only declared in one module is correctly handled (derive)"
	, goToDeclarationOfDeriveGenericFuncMultipleResultsIsCorrectlyHandledFor
		as "go to declaration of a generic function that is declared in two modules is correctly handled (derive)"
	, goToDeclarationOfRecordFieldSingleResultIsCorrectlyHandledFor
		as "go to declaration of a record field that is only declared in one module is correctly handled"
	, goToDeclarationOfRecordFieldMultipleResultsIsCorrectlyHandledFor
		as "go to declaration of a record field that is declared in two modules is correctly handled"
	, goToDeclarationOfClassSingleResultIsCorrectlyHandledFor
		as "go to declaration of a class that is declared in one module is correctly handled"
	, goToDeclarationOfClassMultipleResultsIsCorrectlyHandledFor
		as "go to declaration of a class that is declared in two modules is correctly handled"
	, goToDeclarationOfClassFuncSingleResultIsCorrectlyHandledFor
		as "go to declaration of a class function with a single result is correctly handled"
	, goToDeclarationOfClassFuncMultipleResultsIsCorrectlyHandledFor
		as "go to declaration of a class function with multiple results is correctly handled"
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
	= accUnsafe configIsMissingResultsInErrorLogOnSave`
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

configPathEmpty :: !String !String -> Property
configPathEmpty suite file = accUnsafe configPathEmpty`
where
	configPathEmpty` :: !*World -> (!Property, !*World)
	configPathEmpty` world
		# (Ok curDir, world) = getCurrentDirectory world
		= assertResponseForSaveNotification
			suite
			file
			(expectedDiagnosticsResponseBody curDir)
			world

	expectedDiagnosticsResponseBody :: !String -> String
	expectedDiagnosticsResponseBody curDir =
		concat3
			"{\"jsonrpc\":2.0,\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":\"file://"
			(curDir </> suite </> file)
			"\",\"diagnostics\":[]}}"

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

expectedInitializeResponseBody = "{\"jsonrpc\":2.0,\"id\":1,\"result\":{\"capabilities\":{\"textDocumentSync\":{\"openClose\":true,\"save\":true},\"declarationProvider\":true},\"serverInfo\":{\"name\":\"Eastwood\",\"version\":\"WIP\"}}}"

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


goToDeclarationTest :: !String !String !Position ![!(String,UInt)!] -> Property
goToDeclarationTest suite fileName position expectedFileNamesAndLineNumbers = accUnsafe goToDeclarationTest`
where
	goToDeclarationTest` world
		# (Ok currentDirectory, world) = getCurrentDirectory world
		# expectedFilePathsAndLineNumbers
			= Map (appFst (\s -> currentDirectory </> suite </> s)) expectedFileNamesAndLineNumbers
		# (response, finalOut, world) =
			singleMessageResponse
				suite
				(goToDeclarationRequestBodyFor (currentDirectory </> suite </> fileName) position)
				world
		=	(
				(generateMessage $
					"{\"jsonrpc\":2.0," +++
					dropChars 1 (toString $ serialize $ goToDeclarationResponseBodyFor expectedFilePathsAndLineNumbers)
				)
					=.= response
				/\
				finalOut =.= ?None
			,	world
			)

	goToDeclarationRequestBodyFor filePath {line=(UInt line),character=(UInt char)} =
		concat
			[ "{\"jsonrpc\": \"2.0\", \"id\":\"5\", \"method\":\"textDocument/declaration\",\"params\":{\"textDocument\":{\"uri\":\"file://"
			, filePath
			, "\"},\"position\":{\"line\":"
			, toString line
			, ",\"character\":"
			, toString char
			,"}}}"
			]

	goToDeclarationResponseBodyFor :: ![!(String, UInt)!] -> ResponseMessage
	goToDeclarationResponseBodyFor filesAndLineNumbers =
		{ ResponseMessage
		| id = ?Just $ RequestId (Right "5")
		, result =
			?Just jsonResult
		, error = ?None
		}
	where
		jsonResult :: JSONNode
		jsonResult
			# locations = [! fAndLn \\ fAndLn <- Map fileAndLineToLocation filesAndLineNumbers | isJust fAndLn !]
			= serialize locations
		where
			fileAndLineToLocation :: !(!String, !UInt) -> ?Location
			fileAndLineToLocation (filePath, (UInt lineNr))
				# fileUri = parseURI $ "file://" </> filePath
				| isNone fileUri = ?None
				= ?Just $
					{ Location
					| uri = fromJust fileUri
					, range =
						{ start={line=uint lineNr, character=uint 0}
						, end={line=uint lineNr, character=uint 0}
						}
					}

goToDeclarationOfTypeSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfTypeSingleResultIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_DCL_1
		typeSingleResultPosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 6)!]
where
	typeSingleResultPosition :: Position
	typeSingleResultPosition = {line=uint 6, character=uint 6}

goToDeclarationOfTypeMultipleResultsIsCorrectlyHandledFor :: Property
goToDeclarationOfTypeMultipleResultsIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_DCL_1
		typeMultipleResultsPosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 4), (FILE_GO_TO_DECLARATION_DCL_2, uint 4)!]
where
	typeMultipleResultsPosition :: Position
	typeMultipleResultsPosition = {line=uint 4, character=uint 12}

goToDeclarationOfFuncSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfFuncSingleResultIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_DCL_1
		funcSingleResultPosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 8)!]
where
	funcSingleResultPosition :: Position
	funcSingleResultPosition = {line=uint 8, character=uint 5}

goToDeclarationOfFuncMultipleResultsIsCorrectlyHandledFor :: Property
goToDeclarationOfFuncMultipleResultsIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_DCL_2
		funcMultipleResultsPosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 10), (FILE_GO_TO_DECLARATION_DCL_2, uint 6)!]
where
	funcMultipleResultsPosition :: Position
	funcMultipleResultsPosition = {line=uint 6, character=uint 5}

goToDeclarationOfInfixlFuncSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfInfixlFuncSingleResultIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_ICL_1
		infixFuncSingleResultPosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 35)!]
where
	infixFuncSingleResultPosition :: Position
	infixFuncSingleResultPosition = {line=uint 9, character = uint 56}

goToDeclarationOfInfixrFuncSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfInfixrFuncSingleResultIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_ICL_1
		infixrFuncSingleResultPosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 39)!]
where
	infixrFuncSingleResultPosition :: Position
	infixrFuncSingleResultPosition = {line=uint 3, character = uint 36}

goToDeclarationOfInfixFuncSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfInfixFuncSingleResultIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_ICL_1
		infixFuncSingleResultPosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 41)!]
where
	infixFuncSingleResultPosition :: Position
	infixFuncSingleResultPosition = {line=uint 3, character = uint 27}

goToDeclarationOfInfixlFuncMultipleResultsIsCorrectlyHandledFor :: Property
goToDeclarationOfInfixlFuncMultipleResultsIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_ICL_1
		infixFuncMultipleResultsPosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 37), (FILE_GO_TO_DECLARATION_DCL_2, uint 19)!]
where
	infixFuncMultipleResultsPosition :: Position
	infixFuncMultipleResultsPosition = {line=uint 6, character = uint 41}

goToDeclarationOfInfixlFuncUsedPrefixSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfInfixlFuncUsedPrefixSingleResultIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_ICL_1
		infixFuncSingleResultPosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 35)!]
where
	infixFuncSingleResultPosition :: Position
	infixFuncSingleResultPosition = {line=uint 9, character = uint 41}

goToDeclarationOfInfixlFuncUsedPrefixMultipleResultsIsCorrectlyHandledFor :: Property
goToDeclarationOfInfixlFuncUsedPrefixMultipleResultsIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_ICL_1
		infixFuncMultipleResultsPosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 37), (FILE_GO_TO_DECLARATION_DCL_2, uint 19)!]
where
	infixFuncMultipleResultsPosition :: Position
	infixFuncMultipleResultsPosition = {line=uint 6, character = uint 25}

goToDeclarationOfFuncThatStartsWithUppercaseIsCorrectlyHandledFor :: Property
goToDeclarationOfFuncThatStartsWithUppercaseIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_DCL_1
		funcThatStartsWithUppercasePosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 12)!]
where
	funcThatStartsWithUppercasePosition :: Position
	funcThatStartsWithUppercasePosition = {line=uint 12, character=uint 6}

goToDeclarationOfDeriveGenericFuncSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfDeriveGenericFuncSingleResultIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_DCL_1
		genericDeriveSingleResultPosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 14)!]
where
	genericDeriveSingleResultPosition :: Position
	genericDeriveSingleResultPosition = {line=uint 14, character=uint 12}

goToDeclarationOfDeriveGenericFuncMultipleResultsIsCorrectlyHandledFor :: Property
goToDeclarationOfDeriveGenericFuncMultipleResultsIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_DCL_2
		genericDeriveMultipleResultsPosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 16), (FILE_GO_TO_DECLARATION_DCL_2, uint 8)!]
where
	genericDeriveMultipleResultsPosition :: Position
	genericDeriveMultipleResultsPosition = {line=uint 8, character=uint 12}

goToDeclarationOfRecordFieldSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfRecordFieldSingleResultIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_DCL_1
		recordFieldSingleResultPosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 19)!]
where
	recordFieldSingleResultPosition :: Position
	recordFieldSingleResultPosition = {line=uint 19, character=uint 6}

goToDeclarationOfRecordFieldMultipleResultsIsCorrectlyHandledFor :: Property
goToDeclarationOfRecordFieldMultipleResultsIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_DCL_1
		recordFieldMultipleResultsPosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 25),(FILE_GO_TO_DECLARATION_DCL_2, uint 12)!]
where
	recordFieldMultipleResultsPosition :: Position
	recordFieldMultipleResultsPosition = {line=uint 25, character=uint 6}

goToDeclarationOfClassFuncSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfClassFuncSingleResultIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_DCL_1
		classFuncSingleResultPosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 29)!]
where
	classFuncSingleResultPosition :: Position
	classFuncSingleResultPosition = {line=uint 29, character = uint 6}

goToDeclarationOfClassFuncMultipleResultsIsCorrectlyHandledFor :: Property
goToDeclarationOfClassFuncMultipleResultsIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_DCL_2
		classFuncMultipleResultsPosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 32), (FILE_GO_TO_DECLARATION_DCL_2, uint 16)!]
where
	classFuncMultipleResultsPosition :: Position
	classFuncMultipleResultsPosition = {line=uint 16, character = uint 6}

goToDeclarationOfClassSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfClassSingleResultIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_DCL_1
		classSingleResultPosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 28)!]
where
	classSingleResultPosition :: Position
	classSingleResultPosition = {line=uint 28, character=uint 12}

goToDeclarationOfClassMultipleResultsIsCorrectlyHandledFor :: Property
goToDeclarationOfClassMultipleResultsIsCorrectlyHandledFor =
	goToDeclarationTest
		SUITE_DEFAULT
		FILE_GO_TO_DECLARATION_DCL_2
		classMultipleResultsPosition
		[!(FILE_GO_TO_DECLARATION_DCL_1, uint 31),(FILE_GO_TO_DECLARATION_DCL_2, uint 15)!]
where
	classMultipleResultsPosition :: Position
	classMultipleResultsPosition = {line=uint 15, character=uint 12}