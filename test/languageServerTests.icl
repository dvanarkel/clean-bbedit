module languageServerTests

import StdEnv
import StdOverloadedList
import StdMaybe
import Data.List
import Data.Func, Data.Error
import Data.Tuple
import Data.Either
import Data.Maybe.Gast
import Data.Maybe.GenPrint
import System.Directory
from System.FilePath import </>, <.>, addExtension
from System.Process import :: ProcessIO {..}, checkProcess
import System.Time
import System.Environment
import System._Unsafe
import Text
from Gast import
	:: Property, :: Testoption (Bent), =.=, generic genShow, as, name, :: PrintOption (OutputTestEvents),
	instance Testable Property, instance Testable Bool, ExistsIn, class TestArg,
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
SUITE_CONFIG_MISSING_STDENV :== "suite-config-missing-stdenv"

CLEAN_HOME_ENV_VAR :== "CLEAN_HOME"
LIBS_PATH :== "lib"

// A non-existing file. Used when the actual file doesn't matter, because Eastwood is expected to run into a more
// fundamental edge-case.
FILE_DONT_CARE :== "dontcare.icl"
FILE_OK :== "ok.icl"
FILE_GO_TO_DCL_1 :== "GoToModule1.dcl"
FILE_GO_TO_ICL_1 :== "GoToModule1.icl"
FILE_GO_TO_DCL_2 :== "GoToModule2.dcl"
FILE_GO_TO_ICL_2 :== "GoToModule2.icl"
FILE_URL_ENCODED_MODULE_NAME_DCL :== "UrlEncodedModuleName%60.dcl"

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
		[!("someLib" </> "TestModule.dcl", noDiagnostics), ("someLib" </> "TestModule.icl", noDiagnostics)]
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
	, goToDeclarationOfUsageOfMonoKindedGenericFuncSelectingKindSpecificationIsCorrectlyHandledFor
		as "go to declaration of a generic function when selecting the mono kinded specification of the usage is correctly handled (usage, non special syntax symbol)"
	, goToDeclarationOfUsageOfMonoKindedGenericFuncSelectingSpecialSyntaxSymbolOfKindSpecificationIsCorrectlyHandledFor
		as "go to declaration of a generic function when selecting the mono kinded specification of the usage is correctly handled (usage, special syntax symbol)"
	, goToDeclarationOfUsageOfHigherKindedGenericFuncSelectingKindSpecificationIsCorrectlyHandledFor
		as "go to declaration of a generic function when selecting the higher kinded specification of the usage is correctly handled (usage, non special syntax symbol)"
	, goToDeclarationOfUsageOfHigherKindedGenericFuncSelectingSpecialSyntaxSymbolWithinKindSpecificationIsCorrectlyHandledFor
		as "go to declaration of a generic function when selecting the higher kinded specification of the usage is correctly handled (usage, special syntax symbol)"
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
	, goToDeclarationOfFirstConstructorSameLineAsTypeDefCorrectlyHandledFor
		as "go to declaration of the first constructor on the same line as the type def is correctly handled"
	, goToDeclarationOfSecondConstructorSameLineAsTypeDefCorrectlyHandledFor
		as "go to declaration of the second constructor on the same line as the type def is correctly handled"
	, goToDeclarationOfConstructorPrecededByPipeOnPreviousLineWithTypeDefCorrectlyHandledFor
		as "go to declaration of constructor which is preceded by a pipe in the previous line with a type def is correctly handled"
	, goToDeclarationOfConstructorPrecededByPipeOnPreviousLineWithoutTypeDefCorrectlyHandledFor
		as "go to declaration of constructor which is preceded by a pipe in the previous line without a type def is correctly handled"
	, goToDeclarationOfConstructorPrecededByPipeOnSameLineWithoutTypeDefCorrectlyHandledFor
		as "go to declaration of constructor which is preceded by a pipe on the same line without a type def is correctly handled"
	, goToDeclarationOfConstructorWithArgsPrecededByPipeOnSameLineWithoutTypeDefCorrectlyHandledFor
		as "go to declaration of constructor with arguments preceded by a pipe on the same line without a type def is correctly handled"
	, goToDeclarationOfConstructorWithArgsPrecededByOtherConstructorOnSameLineWithoutTypeDefCorrectlyHandledFor
		as "go to declaration of constructor with arguments preceded by another constructor is correctly handled for"
	, goToDeclarationOfConstructorPrecededByEqualsOnPreviousLineWithTypeDefCorrectlyHandledFor
		as "go to declaration of constructor preceded by an equals sign on the previous line which contains a type def is correctly handled for"
	, goToDeclarationOfConstructorPrecededByEqualsOnSameLineWithoutTypeDefCorrectlyHandledFor
		as "go to declaration of constructor preceded by an equals sign on the same line without a type def is correctly handled"
	, goToDeclarationOfMacroWithoutArgsCorrectlyHandledFor
		as "go to declaration of a macro without arguments is correctly handled"
	, goToDeclarationOfMacroWithArgsCorrectlyHandledFor
		as "go to declaration of a macro with arguments is correctly handled"
	, goToDeclarationOfStdEnvFuncWhenLibraryIsPartOfConfig
		as "go to declaration of function defined in StdEnv when the StdEnv library is included in Eastwood.yml"
	, goToDeclarationOfStdEnvFuncWhenLibraryIsMissingInConfig
		as "go to declaration of function defined in StdEnv when the StdEnv library is not included in Eastwood.yml"
	, goToDeclarationOfTypeSynonymCorrectlyHandledFor
		as "go to declaration of a type synonym is correctly handled"
	, goToDeclarationOfAbstractTypeSynonymCorrectlyHandledFor
		as "go to declaration of an abstract type synonym is correctly handled"
	, goToDeclarationOfNewTypeCorrectlyHandledFor
		as "go to declaration of a newtype is correctly handled"
	, goToDeclarationOfAbstractNewTypeCorrectlyHandledFor
		as "go to declaration of an abstract newtype is correctly handled"
	, goToDeclarationOfRecordTypeInTypeSpecificationForRecordFieldCorrectlyHandledFor
		as "go to declaration of a record field access type specification is correctly handled"
	, goToDeclarationOfTypeDoesNotFindNewTypeWhenNameOfTypeIsInfixOfButDoesNotEqualNewTypeName
		as "go to declaration of a type does not find a newtype when the type name is infix of the newtype name."
	, goToDeclarationOfTypeInModuleWithNameForWhichUrlEncodingAltersName
		as "go to declaration of a type located within a module that has a name for which url encoding alters the name."
	, goToDefinitionOfRecordFieldPrecededByBraceOnPreviousLineCorrectlyHandledFor
		as "go to definition of record field preceded by { on the previous line is correctly handled"
	, goToDefinitionOfRecordFieldPrecededByCommaOnPreviousLineCorrectlyHandledFor
		as "go to definition of record field preceded by , on the previous line is correctly handled"
	, goToDefinitionOfRecordFieldPrecededByCommaOnSameLineCorrectlyHandledFor
		as "go to definition of record field preceded by , on the same line is correctly handled"
	, goToDefinitionOfRecordFieldPrecededByBraceAndRecordDefinitionOnSameLineCorrectlyHandledFor
		as "go to definition of record field preceded by { and record definition on the same line is correctly handled"
	, goToDefinitionOfRecordFieldPrecededByCommaAndRecordDefinitionOnSameLineCorrectlyHandledFor
		as "go to definition of record field preceded by , and record definition on the same line is correctly handled"
	// There is a still an edge case here with lets say [(a,b):gFunc{|*|}] unless a space is added before gFunc.
	, goToDefinitionStopParsingSymbolsOfPrefixFunctionWhenSearchTermDoesNotContainGenericKindSpecificationCorrectlyRemovedFor
		as "go to definition stop parsing symbols when function is prefix and does not contain generic kind specification is correctly handled"
	, goToDefinitionOfTypeAlsoDefinedInOtherIclCorrectlyHandledFor
		as "go to definition of type which is also defined module local in another .icl module is correctly handled for"
	, goToDefinitionOfGenericCorrectlyHandledFor
		as "go to definition of a generic is correctly handled for"
	, goToDefinitionOfNewtypeCorrectlyHandledFor
		as "go to definition of a newtype is correctly handled for"
	, goToDefinitionOfMacroCorrectlyHandledFor
		as "go to definition of a macro is correctly handled for"
	, goToDefinitionOfTypeSynonymCorrectlyHandledFor
		as "go to definition of a type synonym is correctly handled for"
	, goToDefinitionOfClassCorrectlyHandledFor
		as "go to definition of a class is correctly handled for"
	, goToDefinitionOfClassSingleFunctionSyntaxCorrectlyHandledFor
		as "go to definition of a class which has one function and uses special syntax for this reason is correctly handled for"
	, goToDefinitionOfConstructorPrecededByTypeDefCorrectlyHandledFor
		as "go to definition of a constructor which is preceded by a type definition on the same line is correctly handled for"
	, goToDefinitionOfConstructorPrecededByTypeDefAndOtherConstructorCorrectlyHandledFor
		as "go to definition of a constructor which is a preceded by a type definition and another constructor on the same line is correctly handled for"
	, goToDefinitionOfConstructorPrecededByTypeDefAndPipeOnPreviousLineCorrectlyHandledFor
		as "go to definition of a constructor which is preceded by a type def and pipe on the previous line is correctly handled for"
	, goToDefinitionOfConstructorPrecededByPipeOnPreviousLineCorrectlyHandledFor
		as "go to definition of a constructor preceded by pipe on the previous line is correctly handled for"
	, goToDefinitionOfConstructorPrecededByPipeOnSameLineCorrectlyHandledFor
		as "go to definition of a constructor preceded by pipe on the same line is correctly handled for"
	, goToDefinitionOfConstructorWithArgsPrecededByPipeOnSameLineCorrectlyHandledFor
		as "go to definition of a constructor with args that is preceded by a pipe on the same line is correctly handled for"
	, goToDefinitionOfConstructorWithArgsPrecededByPipeAndOtherConstructorOnSameLineCorrectlyHandledFor
		as "go to definition of a constructor with args that is preceded by a pipe and another constructor on the same line is correctly handled for"
	, goToDefinitionOfConstructorPrecededByEqualsOnPreviousLineCorrectlyHandledFor
		as "go to definition of a constructor that is preceded by = (no type def) on the previous line is correctly handled for"
	, goToDefinitionOfConstructorPrecededByEqualsOnSameLineCorrectlyHandledFor
		as "go to definition of a constructor that is preceded by = (no type def) on the same line is correctly handled for"
	, goToDefinitionOfFuncNoTypeAnnotationEqualsSameLineCorrectlyHandledFor
		as "go to definition of a function without a type annotation with the = on the same line is correctly handled for"
	, goToDefinitionOfFuncNoTypeAnnotationEqualsNextLineCorrectlyHandledFor
		as "go to definition of a function without a type annotation with the = on the next line is correctly handled for"
	, goToDefinitionOfFuncNoTypeAnnotationLetNextLineCorrectlyHandledFor
		as "go to definition of a function without a type annotation with a # on the next line is correctly handled for"
	, goToDefinitionOfFuncNoTypeAnnotationGuardNextLineCorrectlyHandledFor
		as "go to definition of a function without a type annotation with a | on the next line is correctly handled for"
	// Copy of a different test but this test was added to describe the behavior of the current implementation.
	, goToDefinitionOfFuncWithTypeAnnotationResultsInTypeDefinitionLineOnly
		as "go to definition of a function with a type annotation only yields a result for the line containing the type definition"
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

expectedInitializeResponseBody = "{\"jsonrpc\":2.0,\"id\":1,\"result\":{\"capabilities\":{\"textDocumentSync\":{\"openClose\":true,\"save\":true},\"declarationProvider\":true,\"definitionProvider\":true},\"serverInfo\":{\"name\":\"Eastwood\",\"version\":\"WIP\"}}}"

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

derive gPrint UInt
derive genShow UInt
derive ggen UInt

:: GotoKind = Declaration | Definition

goToTest :: !GotoKind !String !String !Position ![!(String,UInt)!] -> Property
goToTest gotoKind suite fileName position expectedFileNamesAndLineNumbers
	= accUnsafe goToTest`
where
	goToTest` world
		# (Ok currentDirectory, world) = getCurrentDirectory world
		# expectedFilePathsAndLineNumbers
			= Map (appFst (\s -> currentDirectory </> suite </> s)) expectedFileNamesAndLineNumbers
		= (goToTestAbsolutePaths
			gotoKind
			suite
			(currentDirectory </> suite </> fileName)
			position
			expectedFilePathsAndLineNumbers, world)

goToTestAbsolutePaths :: !GotoKind !String !FilePath !Position ![(FilePath,UInt)] -> Property
goToTestAbsolutePaths gotoKind suite pathForRequest position expectedFilePathsAndLineNumbers
	= accUnsafe goToTestAbsolutePaths`
where
	goToTestAbsolutePaths` world
		# expectedFilePathsAndLineNumbersPerms
			= map (\e -> [!x \\ x <- e!]) $
				permutations
					[expectedFilePathAndLineNumber \\ expectedFilePathAndLineNumber <- expectedFilePathsAndLineNumbers]
		# (response, finalOut, world) =
			singleMessageResponse
				suite
				(goToRequestBodyFor gotoKind pathForRequest position)
				world
		=	( 	ExistsIn
					(\expectedFilePathsAndLineNumbersPerm ->
						(generateMessage $
							"{\"jsonrpc\":2.0," +++
							dropChars
								1
								(toString $ serialize $
									goToResponseBodyFor expectedFilePathsAndLineNumbersPerm
								)
						)
						=.= response
					)
					expectedFilePathsAndLineNumbersPerms
				/\
				finalOut =.= ?None
			,	world
			)

	goToRequestBodyFor :: !GotoKind !String !Position -> String
	goToRequestBodyFor gotoKind filePath {line=(UInt line),character=(UInt char)} =
		concat
			[ "{\"jsonrpc\": \"2.0\", \"id\":\"5\", \"method\":\""
			,"textDocument/"
			, if (gotoKind=:Declaration) "declaration" "definition"
			, "\",\"params\":{\"textDocument\":{\"uri\":\"file://"
			, filePath
			, "\"},\"position\":{\"line\":"
			, toString line
			, ",\"character\":"
			, toString char
			,"}}}"
			]

	goToResponseBodyFor :: ![!(String, UInt)!] -> ResponseMessage
	goToResponseBodyFor filesAndLineNumbers =
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
goToDeclarationOfTypeSingleResultIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		typeSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 6)!]
where
	typeSingleResultPosition :: Position
	typeSingleResultPosition = {line=uint 6, character=uint 6}

goToDeclarationOfTypeMultipleResultsIsCorrectlyHandledFor :: Property
goToDeclarationOfTypeMultipleResultsIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		typeMultipleResultsPosition
		[!(FILE_GO_TO_DCL_1, uint 4), (FILE_GO_TO_DCL_2, uint 4)!]
where
	typeMultipleResultsPosition :: Position
	typeMultipleResultsPosition = {line=uint 4, character=uint 12}

goToDeclarationOfFuncSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfFuncSingleResultIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		funcSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 8)!]
where
	funcSingleResultPosition :: Position
	funcSingleResultPosition = {line=uint 8, character=uint 5}

goToDeclarationOfFuncMultipleResultsIsCorrectlyHandledFor :: Property
goToDeclarationOfFuncMultipleResultsIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_2
		funcMultipleResultsPosition
		[!(FILE_GO_TO_DCL_1, uint 10), (FILE_GO_TO_DCL_2, uint 6)!]
where
	funcMultipleResultsPosition :: Position
	funcMultipleResultsPosition = {line=uint 6, character=uint 5}

goToDeclarationOfInfixlFuncSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfInfixlFuncSingleResultIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		infixFuncSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 35)!]
where
	infixFuncSingleResultPosition :: Position
	infixFuncSingleResultPosition = {line=uint 9, character = uint 56}

goToDeclarationOfInfixrFuncSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfInfixrFuncSingleResultIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		infixrFuncSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 39)!]
where
	infixrFuncSingleResultPosition :: Position
	infixrFuncSingleResultPosition = {line=uint 3, character = uint 36}

goToDeclarationOfInfixFuncSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfInfixFuncSingleResultIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		infixFuncSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 41)!]
where
	infixFuncSingleResultPosition :: Position
	infixFuncSingleResultPosition = {line=uint 3, character = uint 27}

goToDeclarationOfInfixlFuncMultipleResultsIsCorrectlyHandledFor :: Property
goToDeclarationOfInfixlFuncMultipleResultsIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		infixFuncMultipleResultsPosition
		[!(FILE_GO_TO_DCL_1, uint 37), (FILE_GO_TO_DCL_2, uint 19)!]
where
	infixFuncMultipleResultsPosition :: Position
	infixFuncMultipleResultsPosition = {line=uint 6, character = uint 41}

goToDeclarationOfInfixlFuncUsedPrefixSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfInfixlFuncUsedPrefixSingleResultIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		infixFuncSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 35)!]
where
	infixFuncSingleResultPosition :: Position
	infixFuncSingleResultPosition = {line=uint 9, character = uint 41}

goToDeclarationOfInfixlFuncUsedPrefixMultipleResultsIsCorrectlyHandledFor :: Property
goToDeclarationOfInfixlFuncUsedPrefixMultipleResultsIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		infixFuncMultipleResultsPosition
		[!(FILE_GO_TO_DCL_1, uint 37), (FILE_GO_TO_DCL_2, uint 19)!]
where
	infixFuncMultipleResultsPosition :: Position
	infixFuncMultipleResultsPosition = {line=uint 6, character = uint 25}

goToDeclarationOfFuncThatStartsWithUppercaseIsCorrectlyHandledFor :: Property
goToDeclarationOfFuncThatStartsWithUppercaseIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		funcThatStartsWithUppercasePosition
		[!(FILE_GO_TO_DCL_1, uint 12)!]
where
	funcThatStartsWithUppercasePosition :: Position
	funcThatStartsWithUppercasePosition = {line=uint 12, character=uint 6}

goToDeclarationOfDeriveGenericFuncSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfDeriveGenericFuncSingleResultIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		genericDeriveSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 14)!]
where
	genericDeriveSingleResultPosition :: Position
	genericDeriveSingleResultPosition = {line=uint 14, character=uint 12}

goToDeclarationOfDeriveGenericFuncMultipleResultsIsCorrectlyHandledFor :: Property
goToDeclarationOfDeriveGenericFuncMultipleResultsIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_2
		genericDeriveMultipleResultsPosition
		[!(FILE_GO_TO_DCL_1, uint 16), (FILE_GO_TO_DCL_2, uint 8)!]
where
	genericDeriveMultipleResultsPosition :: Position
	genericDeriveMultipleResultsPosition = {line=uint 8, character=uint 12}

goToDeclarationOfUsageOfMonoKindedGenericFuncSelectingKindSpecificationIsCorrectlyHandledFor :: Property
goToDeclarationOfUsageOfMonoKindedGenericFuncSelectingKindSpecificationIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		genericFuncUsagePosition
		[!(FILE_GO_TO_DCL_1, uint 59)!]
where
	genericFuncUsagePosition :: Position
	genericFuncUsagePosition = {line=uint 26, character=uint 33}

goToDeclarationOfUsageOfMonoKindedGenericFuncSelectingSpecialSyntaxSymbolOfKindSpecificationIsCorrectlyHandledFor
	:: Property
goToDeclarationOfUsageOfMonoKindedGenericFuncSelectingSpecialSyntaxSymbolOfKindSpecificationIsCorrectlyHandledFor
	=:	goToTest
			Declaration
			SUITE_DEFAULT
			FILE_GO_TO_ICL_1
			genericFuncUsagePosition
			[!(FILE_GO_TO_DCL_1, uint 59)!]
where
	genericFuncUsagePosition :: Position
	genericFuncUsagePosition = {line=uint 26, character=uint 34}

goToDeclarationOfUsageOfHigherKindedGenericFuncSelectingKindSpecificationIsCorrectlyHandledFor :: Property
goToDeclarationOfUsageOfHigherKindedGenericFuncSelectingKindSpecificationIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		genericFuncUsagePosition
		[!(FILE_GO_TO_DCL_1, uint 59)!]
where
	genericFuncUsagePosition :: Position
	genericFuncUsagePosition = {line=uint 31, character=uint 49}

goToDeclarationOfUsageOfHigherKindedGenericFuncSelectingSpecialSyntaxSymbolWithinKindSpecificationIsCorrectlyHandledFor
	:: Property
goToDeclarationOfUsageOfHigherKindedGenericFuncSelectingSpecialSyntaxSymbolWithinKindSpecificationIsCorrectlyHandledFor
	=:	goToTest
			Declaration
			SUITE_DEFAULT
			FILE_GO_TO_ICL_1
			genericFuncUsagePosition
			[!(FILE_GO_TO_DCL_1, uint 59)!]
where
	genericFuncUsagePosition :: Position
	genericFuncUsagePosition = {line=uint 31, character=uint 50}


goToDeclarationOfRecordFieldSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfRecordFieldSingleResultIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		recordFieldSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 19)!]
where
	recordFieldSingleResultPosition :: Position
	recordFieldSingleResultPosition = {line=uint 19, character=uint 6}

goToDeclarationOfRecordFieldMultipleResultsIsCorrectlyHandledFor :: Property
goToDeclarationOfRecordFieldMultipleResultsIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		recordFieldMultipleResultsPosition
		[!(FILE_GO_TO_DCL_1, uint 25),(FILE_GO_TO_DCL_2, uint 12)!]
where
	recordFieldMultipleResultsPosition :: Position
	recordFieldMultipleResultsPosition = {line=uint 25, character=uint 6}

goToDeclarationOfClassFuncSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfClassFuncSingleResultIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		classFuncSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 29)!]
where
	classFuncSingleResultPosition :: Position
	classFuncSingleResultPosition = {line=uint 29, character = uint 6}

goToDeclarationOfClassFuncMultipleResultsIsCorrectlyHandledFor :: Property
goToDeclarationOfClassFuncMultipleResultsIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_2
		classFuncMultipleResultsPosition
		[!(FILE_GO_TO_DCL_1, uint 32), (FILE_GO_TO_DCL_2, uint 16)!]
where
	classFuncMultipleResultsPosition :: Position
	classFuncMultipleResultsPosition = {line=uint 16, character = uint 6}

goToDeclarationOfClassSingleResultIsCorrectlyHandledFor :: Property
goToDeclarationOfClassSingleResultIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		classSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 28)!]
where
	classSingleResultPosition :: Position
	classSingleResultPosition = {line=uint 28, character=uint 12}

goToDeclarationOfClassMultipleResultsIsCorrectlyHandledFor :: Property
goToDeclarationOfClassMultipleResultsIsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_2
		classMultipleResultsPosition
		[!(FILE_GO_TO_DCL_1, uint 31),(FILE_GO_TO_DCL_2, uint 15)!]
where
	classMultipleResultsPosition :: Position
	classMultipleResultsPosition = {line=uint 15, character=uint 12}

goToDeclarationOfFirstConstructorSameLineAsTypeDefCorrectlyHandledFor :: Property
goToDeclarationOfFirstConstructorSameLineAsTypeDefCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		constructorPosition
		[!(FILE_GO_TO_DCL_1, uint 43)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 43, character=uint 39}

goToDeclarationOfSecondConstructorSameLineAsTypeDefCorrectlyHandledFor :: Property
goToDeclarationOfSecondConstructorSameLineAsTypeDefCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		constructorPosition
		[!(FILE_GO_TO_DCL_1, uint 43)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 43, character=uint 47}

goToDeclarationOfConstructorPrecededByPipeOnPreviousLineWithTypeDefCorrectlyHandledFor :: Property
goToDeclarationOfConstructorPrecededByPipeOnPreviousLineWithTypeDefCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		constructorPosition
		[!(FILE_GO_TO_DCL_1, uint 44)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 44, character=uint 7}

goToDeclarationOfConstructorPrecededByPipeOnPreviousLineWithoutTypeDefCorrectlyHandledFor :: Property
goToDeclarationOfConstructorPrecededByPipeOnPreviousLineWithoutTypeDefCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		constructorPosition
		[!(FILE_GO_TO_DCL_1, uint 45)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 45, character=uint 7}

goToDeclarationOfConstructorPrecededByPipeOnSameLineWithoutTypeDefCorrectlyHandledFor :: Property
goToDeclarationOfConstructorPrecededByPipeOnSameLineWithoutTypeDefCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		constructorPosition
		[!(FILE_GO_TO_DCL_1, uint 46)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 46, character=uint 7}

goToDeclarationOfConstructorWithArgsPrecededByPipeOnSameLineWithoutTypeDefCorrectlyHandledFor :: Property
goToDeclarationOfConstructorWithArgsPrecededByPipeOnSameLineWithoutTypeDefCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		constructorPosition
		[!(FILE_GO_TO_DCL_1, uint 47)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 47, character=uint 7}

goToDeclarationOfConstructorWithArgsPrecededByOtherConstructorOnSameLineWithoutTypeDefCorrectlyHandledFor :: Property
goToDeclarationOfConstructorWithArgsPrecededByOtherConstructorOnSameLineWithoutTypeDefCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		constructorPosition
		[!(FILE_GO_TO_DCL_1, uint 47)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 47, character=uint 30}

goToDeclarationOfConstructorPrecededByEqualsOnSameLineWithoutTypeDefCorrectlyHandledFor :: Property
goToDeclarationOfConstructorPrecededByEqualsOnSameLineWithoutTypeDefCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		constructorPosition
		[!(FILE_GO_TO_DCL_1, uint 50)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 50, character=uint 10}

goToDeclarationOfConstructorPrecededByEqualsOnPreviousLineWithTypeDefCorrectlyHandledFor :: Property
goToDeclarationOfConstructorPrecededByEqualsOnPreviousLineWithTypeDefCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		constructorPosition
		[!(FILE_GO_TO_DCL_1, uint 53)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 53, character=uint 7}

goToDeclarationOfMacroWithArgsCorrectlyHandledFor :: Property
goToDeclarationOfMacroWithArgsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		macroPosition
		[!(FILE_GO_TO_DCL_1, uint 81)!]
where
	macroPosition :: Position
	macroPosition = {line=uint 81, character=uint 7}

goToDeclarationOfMacroWithoutArgsCorrectlyHandledFor :: Property
goToDeclarationOfMacroWithoutArgsCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		macroPosition
		[!(FILE_GO_TO_DCL_1, uint 83)!]
where
	macroPosition :: Position
	macroPosition = {line=uint 83, character=uint 5}

goToDeclarationOfStdEnvFuncWhenLibraryIsPartOfConfig :: Property
goToDeclarationOfStdEnvFuncWhenLibraryIsPartOfConfig =:
	accUnsafe \w
	# (currentDirectory, w) = appFst fromOk $ getCurrentDirectory w
	# (cleanHomePath, w) = appFst fromJust $ getEnvironmentVariable CLEAN_HOME_ENV_VAR w
	->	(goToTestAbsolutePaths
			Declaration
			SUITE_DEFAULT
			(currentDirectory </> SUITE_DEFAULT </> FILE_GO_TO_ICL_1)
			stdEnvFuncPosition
			[(cleanHomePath </> LIBS_PATH </> "StdEnv" </> "StdBool" <.> "dcl", uint 18)]
		, w
		)
where
	stdEnvFuncPosition :: Position
	stdEnvFuncPosition = {line=uint 64, character=uint 15}

goToDeclarationOfStdEnvFuncWhenLibraryIsMissingInConfig :: Property
goToDeclarationOfStdEnvFuncWhenLibraryIsMissingInConfig =:
	accUnsafe \w
	# (currentDirectory, w) = appFst fromOk $ getCurrentDirectory w
	# (cleanHomePath, w) = appFst fromJust $ getEnvironmentVariable CLEAN_HOME_ENV_VAR w
	->	(goToTestAbsolutePaths
			Declaration
			SUITE_CONFIG_MISSING_STDENV
			(currentDirectory </> SUITE_CONFIG_MISSING_STDENV </> FILE_GO_TO_ICL_1)
			stdEnvFuncPosition
			[]
		, w
		)
where
	stdEnvFuncPosition :: Position
	stdEnvFuncPosition = {line=uint 5, character=uint 15}

goToDeclarationOfTypeSynonymCorrectlyHandledFor :: Property
goToDeclarationOfTypeSynonymCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		typeSynonymPosition
		[!(FILE_GO_TO_DCL_1, uint 87)!]
where
	typeSynonymPosition :: Position
	typeSynonymPosition = {line=uint 87, character=uint 10}

goToDeclarationOfAbstractTypeSynonymCorrectlyHandledFor :: Property
goToDeclarationOfAbstractTypeSynonymCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		typeSynonymPosition
		[!(FILE_GO_TO_DCL_1, uint 88)!]
where
	typeSynonymPosition :: Position
	typeSynonymPosition = {line=uint 88, character=uint 10}

goToDeclarationOfNewTypeCorrectlyHandledFor :: Property
goToDeclarationOfNewTypeCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		newtypePosition
		[!(FILE_GO_TO_DCL_1, uint 90)!]
where
	newtypePosition :: Position
	newtypePosition = {line=uint 90, character=uint 8}

goToDeclarationOfAbstractNewTypeCorrectlyHandledFor :: Property
goToDeclarationOfAbstractNewTypeCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		newtypePosition
		[!(FILE_GO_TO_DCL_1, uint 91)!]
where
	newtypePosition :: Position
	newtypePosition = {line=uint 91, character=uint 8}

goToDeclarationOfRecordTypeInTypeSpecificationForRecordFieldCorrectlyHandledFor :: Property
goToDeclarationOfRecordTypeInTypeSpecificationForRecordFieldCorrectlyHandledFor =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		recordFieldTypeSpecificationPosition
		[!(FILE_GO_TO_DCL_1, uint 18)!]
where
	recordFieldTypeSpecificationPosition :: Position
	recordFieldTypeSpecificationPosition = {line=uint 106, character=uint 37}

goToDeclarationOfTypeDoesNotFindNewTypeWhenNameOfTypeIsInfixOfButDoesNotEqualNewTypeName :: Property
goToDeclarationOfTypeDoesNotFindNewTypeWhenNameOfTypeIsInfixOfButDoesNotEqualNewTypeName =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		typeDefPosition
		[!(FILE_GO_TO_DCL_1, uint 93)!]
where
	typeDefPosition :: Position
	typeDefPosition = {line=uint 93, character=uint 6}

goToDeclarationOfTypeInModuleWithNameForWhichUrlEncodingAltersName :: Property
goToDeclarationOfTypeInModuleWithNameForWhichUrlEncodingAltersName =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_URL_ENCODED_MODULE_NAME_DCL
		typeDefPosition
		[!(FILE_URL_ENCODED_MODULE_NAME_DCL, uint 2)!]
where
	typeDefPosition :: Position
	typeDefPosition = {line=uint 2, character=uint 7}

goToDefinitionOfRecordFieldPrecededByBraceOnPreviousLineCorrectlyHandledFor :: Property
goToDefinitionOfRecordFieldPrecededByBraceOnPreviousLineCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		recordFieldPosition
		[!(FILE_GO_TO_ICL_1, uint 71)!]
where
	recordFieldPosition :: Position
	recordFieldPosition = {line=uint 71, character=uint 13}

goToDefinitionOfRecordFieldPrecededByCommaOnPreviousLineCorrectlyHandledFor :: Property
goToDefinitionOfRecordFieldPrecededByCommaOnPreviousLineCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		recordFieldPosition
		[!(FILE_GO_TO_ICL_1, uint 72)!]
where
	recordFieldPosition :: Position
	recordFieldPosition = {line=uint 72, character=uint 13}

goToDefinitionOfRecordFieldPrecededByCommaOnSameLineCorrectlyHandledFor :: Property
goToDefinitionOfRecordFieldPrecededByCommaOnSameLineCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		recordFieldPosition
		[!(FILE_GO_TO_ICL_1, uint 73)!]
where
	recordFieldPosition :: Position
	recordFieldPosition = {line=uint 73, character=uint 13}

goToDefinitionOfRecordFieldPrecededByBraceAndRecordDefinitionOnSameLineCorrectlyHandledFor :: Property
goToDefinitionOfRecordFieldPrecededByBraceAndRecordDefinitionOnSameLineCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		recordFieldPosition
		[!(FILE_GO_TO_ICL_1, uint 76)!]
where
	recordFieldPosition :: Position
	recordFieldPosition = {line=uint 76, character=uint 25}

goToDefinitionOfRecordFieldPrecededByCommaAndRecordDefinitionOnSameLineCorrectlyHandledFor :: Property
goToDefinitionOfRecordFieldPrecededByCommaAndRecordDefinitionOnSameLineCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		recordFieldPosition
		[!(FILE_GO_TO_ICL_1, uint 76)!]
where
	recordFieldPosition :: Position
	recordFieldPosition = {line=uint 76, character=uint 54}

goToDefinitionStopParsingSymbolsOfPrefixFunctionWhenSearchTermDoesNotContainGenericKindSpecificationCorrectlyRemovedFor
	:: Property
goToDefinitionStopParsingSymbolsOfPrefixFunctionWhenSearchTermDoesNotContainGenericKindSpecificationCorrectlyRemovedFor
	=:
		goToTest
			Definition
			SUITE_DEFAULT
			FILE_GO_TO_ICL_1
			recordFieldPosition
			[!(FILE_GO_TO_ICL_1, uint 78)!]
where
	recordFieldPosition :: Position
	recordFieldPosition = {line=uint 79, character=uint 34}

goToDefinitionOfTypeAlsoDefinedInOtherIclCorrectlyHandledFor :: Property
goToDefinitionOfTypeAlsoDefinedInOtherIclCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		typePosition
		[!(FILE_GO_TO_ICL_1, uint 76), (FILE_GO_TO_ICL_2, uint 8)!]
where
	typePosition :: Position
	typePosition = {line=uint 76, character=uint 10}

goToDefinitionOfGenericCorrectlyHandledFor :: Property
goToDefinitionOfGenericCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		genericPosition
		[!(FILE_GO_TO_ICL_1, uint 34)!]
where
	genericPosition :: Position
	genericPosition = {line=uint 34, character=uint 11}

goToDefinitionOfNewtypeCorrectlyHandledFor :: Property
goToDefinitionOfNewtypeCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		newtypePosition
		[!(FILE_GO_TO_ICL_1, uint 82)!]
where
	newtypePosition :: Position
	newtypePosition = {line=uint 82, character=uint 8}

goToDefinitionOfMacroCorrectlyHandledFor :: Property
goToDefinitionOfMacroCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		macroPosition
		[!(FILE_GO_TO_ICL_1, uint 84)!]
where
	macroPosition :: Position
	macroPosition = {line=uint 84, character=uint 1}

goToDefinitionOfTypeSynonymCorrectlyHandledFor :: Property
goToDefinitionOfTypeSynonymCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		typeSynonymPosition
		[!(FILE_GO_TO_ICL_1, uint 86)!]
where
	typeSynonymPosition :: Position
	typeSynonymPosition = {line=uint 86, character=uint 8}

goToDefinitionOfClassCorrectlyHandledFor :: Property
goToDefinitionOfClassCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		classPosition
		[!(FILE_GO_TO_ICL_1, uint 88)!]
where
	classPosition :: Position
	classPosition = {line=uint 88, character=uint 11}

goToDefinitionOfClassSingleFunctionSyntaxCorrectlyHandledFor :: Property
goToDefinitionOfClassSingleFunctionSyntaxCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		classPosition
		[!(FILE_GO_TO_ICL_1, uint 91)!]
where
	classPosition :: Position
	classPosition = {line=uint 91, character=uint 11}

goToDefinitionOfConstructorPrecededByTypeDefCorrectlyHandledFor :: Property
goToDefinitionOfConstructorPrecededByTypeDefCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		constructorPosition
		[!(FILE_GO_TO_ICL_1, uint 93)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 93, character=uint 34}

goToDefinitionOfConstructorPrecededByTypeDefAndOtherConstructorCorrectlyHandledFor :: Property
goToDefinitionOfConstructorPrecededByTypeDefAndOtherConstructorCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		constructorPosition
		[!(FILE_GO_TO_ICL_1, uint 93)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 93, character=uint 47}

goToDefinitionOfConstructorPrecededByTypeDefAndPipeOnPreviousLineCorrectlyHandledFor :: Property
goToDefinitionOfConstructorPrecededByTypeDefAndPipeOnPreviousLineCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		constructorPosition
		[!(FILE_GO_TO_ICL_1, uint 94)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 94, character=uint 8}

goToDefinitionOfConstructorPrecededByPipeOnPreviousLineCorrectlyHandledFor :: Property
goToDefinitionOfConstructorPrecededByPipeOnPreviousLineCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		constructorPosition
		[!(FILE_GO_TO_ICL_1, uint 95)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 95, character=uint 8}

goToDefinitionOfConstructorPrecededByPipeOnSameLineCorrectlyHandledFor :: Property
goToDefinitionOfConstructorPrecededByPipeOnSameLineCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		constructorPosition
		[!(FILE_GO_TO_ICL_1, uint 96)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 96, character=uint 8}

goToDefinitionOfConstructorWithArgsPrecededByPipeOnSameLineCorrectlyHandledFor :: Property
goToDefinitionOfConstructorWithArgsPrecededByPipeOnSameLineCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		constructorPosition
		[!(FILE_GO_TO_ICL_1, uint 97)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 97, character=uint 8}

goToDefinitionOfConstructorWithArgsPrecededByPipeAndOtherConstructorOnSameLineCorrectlyHandledFor :: Property
goToDefinitionOfConstructorWithArgsPrecededByPipeAndOtherConstructorOnSameLineCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		constructorPosition
		[!(FILE_GO_TO_ICL_1, uint 97)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 97, character=uint 36}

goToDefinitionOfConstructorPrecededByEqualsOnSameLineCorrectlyHandledFor :: Property
goToDefinitionOfConstructorPrecededByEqualsOnSameLineCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		constructorPosition
		[!(FILE_GO_TO_ICL_1, uint 100)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 100, character=uint 8}

goToDefinitionOfConstructorPrecededByEqualsOnPreviousLineCorrectlyHandledFor :: Property
goToDefinitionOfConstructorPrecededByEqualsOnPreviousLineCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		constructorPosition
		[!(FILE_GO_TO_ICL_1, uint 103)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 103, character=uint 8}

goToDefinitionOfFuncNoTypeAnnotationEqualsSameLineCorrectlyHandledFor :: Property
goToDefinitionOfFuncNoTypeAnnotationEqualsSameLineCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		funcPosition
		[!(FILE_GO_TO_ICL_1, uint 108)!]
where
	funcPosition :: Position
	funcPosition = {line=uint 108, character=uint 5}

goToDefinitionOfFuncNoTypeAnnotationEqualsNextLineCorrectlyHandledFor :: Property
goToDefinitionOfFuncNoTypeAnnotationEqualsNextLineCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		funcPosition
		[!(FILE_GO_TO_ICL_1, uint 110)!]
where
	funcPosition :: Position
	funcPosition = {line=uint 110, character=uint 5}

goToDefinitionOfFuncNoTypeAnnotationLetNextLineCorrectlyHandledFor :: Property
goToDefinitionOfFuncNoTypeAnnotationLetNextLineCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		funcPosition
		[!(FILE_GO_TO_ICL_1, uint 113)!]
where
	funcPosition :: Position
	funcPosition = {line=uint 113, character=uint 5}

goToDefinitionOfFuncNoTypeAnnotationGuardNextLineCorrectlyHandledFor :: Property
goToDefinitionOfFuncNoTypeAnnotationGuardNextLineCorrectlyHandledFor =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		funcPosition
		[!(FILE_GO_TO_ICL_1, uint 117)!]
where
	funcPosition :: Position
	funcPosition = {line=uint 117, character=uint 5}

goToDefinitionOfFuncWithTypeAnnotationResultsInTypeDefinitionLineOnly :: Property
goToDefinitionOfFuncWithTypeAnnotationResultsInTypeDefinitionLineOnly =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		funcPosition
		[!(FILE_GO_TO_ICL_1, uint 78)!]
where
	funcPosition :: Position
	funcPosition = {line=uint 79, character=uint 5}
