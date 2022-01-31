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
	, goToDeclarationOfTypeSingleResultIsCorrectlyHandled
		as "go to declaration of a type that is only declared in one module is correctly handled"
	, goToDeclarationOfTypeMultipleResultsIsCorrectlyHandled
		as "go to declaration of a type that is declared in two modules is correctly handled"
	, goToDeclarationOfFuncSingleResultIsCorrectlyHandled
		as "go to declaration of a function that is only declared in one module is correctly handled"
	, goToDeclarationOfFuncMultipleResultsIsCorrectlyHandled
		as "go to declaration of a function that is declared in two modules is correctly handled"
	, goToDeclarationOfInfixlFuncSingleResultIsCorrectlyHandled
		as "go to declaration of a infixl function that is only declared in one module is correctly handled"
	, goToDeclarationOfInfixrFuncSingleResultIsCorrectlyHandled
		as "go to declaration of a infixr function is correctly handled."
	, goToDeclarationOfInfixFuncSingleResultIsCorrectlyHandled
		as "go to declaration of a infix function is correctly handled."
	, goToDeclarationOfInfixlFuncMultipleResultsIsCorrectlyHandled
		as "go to declaration of a infixl function that is declared in two modules is correctly handled"
	, goToDeclarationOfInfixlFuncUsedPrefixSingleResultIsCorrectlyHandled
		as "go to declaration of a infixl function used prefix that is only declared in one module is correctly handled"
	, goToDeclarationOfInfixlFuncUsedPrefixMultipleResultsIsCorrectlyHandled
		as "go to declaration of a infixl function used prefix that is declared in two modules is correctly handled"
	, goToDeclarationOfFuncThatStartsWithUppercaseIsCorrectlyHandled
		as "go to declaration of a function whose name starts with an uppercase letter is correctly handled"
	, goToDeclarationOfDeriveGenericFuncSingleResultIsCorrectlyHandled
		as "go to declaration of a generic function that is only declared in one module is correctly handled (derive)"
	, goToDeclarationOfDeriveGenericFuncMultipleResultsIsCorrectlyHandled
		as "go to declaration of a generic function that is declared in two modules is correctly handled (derive)"
	, goToDeclarationOfUsageOfMonoKindedGenericFuncSelectingKindSpecificationIsCorrectlyHandled
		as "go to declaration of a generic function when selecting the mono kinded specification of the usage is correctly handled (usage, non special syntax symbol)"
	, goToDeclarationOfUsageOfMonoKindedGenericFuncSelectingSpecialSyntaxSymbolOfKindSpecificationIsCorrectlyHandled
		as "go to declaration of a generic function when selecting the mono kinded specification of the usage is correctly handled (usage, special syntax symbol)"
	, goToDeclarationOfUsageOfHigherKindedGenericFuncSelectingKindSpecificationIsCorrectlyHandled
		as "go to declaration of a generic function when selecting the higher kinded specification of the usage is correctly handled (usage, non special syntax symbol)"
	, goToDeclarationOfUsageOfHigherKindedGenericFuncSelectingSpecialSyntaxSymbolWithinKindSpecificationIsCorrectlyHandled
		as "go to declaration of a generic function when selecting the higher kinded specification of the usage is correctly handled (usage, special syntax symbol)"
	, goToDeclarationOfRecordFieldSingleResultIsCorrectlyHandled
		as "go to declaration of a record field that is only declared in one module is correctly handled"
	, goToDeclarationOfRecordFieldMultipleResultsIsCorrectlyHandled
		as "go to declaration of a record field that is declared in two modules is correctly handled"
	, goToDeclarationOfClassSingleResultIsCorrectlyHandled
		as "go to declaration of a class that is declared in one module is correctly handled"
	, goToDeclarationOfClassMultipleResultsIsCorrectlyHandled
		as "go to declaration of a class that is declared in two modules is correctly handled"
	, goToDeclarationOfClassFuncSingleResultIsCorrectlyHandled
		as "go to declaration of a class function with a single result is correctly handled"
	, goToDeclarationOfClassFuncMultipleResultsIsCorrectlyHandled
		as "go to declaration of a class function with multiple results is correctly handled"
	, goToDeclarationOfFirstConstructorSameLineAsTypeDefCorrectlyHandled
		as "go to declaration of the first constructor on the same line as the type def is correctly handled"
	, goToDeclarationOfSecondConstructorSameLineAsTypeDefCorrectlyHandled
		as "go to declaration of the second constructor on the same line as the type def is correctly handled"
	, goToDeclarationOfConstructorPrecededByPipeOnPreviousLineWithTypeDefCorrectlyHandled
		as "go to declaration of constructor which is preceded by a pipe in the previous line with a type def is correctly handled"
	, goToDeclarationOfConstructorPrecededByPipeOnPreviousLineWithoutTypeDefCorrectlyHandled
		as "go to declaration of constructor which is preceded by a pipe in the previous line without a type def is correctly handled"
	, goToDeclarationOfConstructorPrecededByPipeOnSameLineWithoutTypeDefCorrectlyHandled
		as "go to declaration of constructor which is preceded by a pipe on the same line without a type def is correctly handled"
	, goToDeclarationOfConstructorWithArgsPrecededByPipeOnSameLineWithoutTypeDefCorrectlyHandled
		as "go to declaration of constructor with arguments preceded by a pipe on the same line without a type def is correctly handled"
	, goToDeclarationOfConstructorWithArgsPrecededByOtherConstructorOnSameLineWithoutTypeDefCorrectlyHandled
		as "go to declaration of constructor with arguments preceded by another constructor is correctly handled for"
	, goToDeclarationOfConstructorPrecededByEqualsOnPreviousLineWithTypeDefCorrectlyHandled
		as "go to declaration of constructor preceded by an equals sign on the previous line which contains a type def is correctly handled for"
	, goToDeclarationOfConstructorPrecededByEqualsOnSameLineWithoutTypeDefCorrectlyHandled
		as "go to declaration of constructor preceded by an equals sign on the same line without a type def is correctly handled"
	, goToDeclarationOfMacroWithoutArgsCorrectlyHandled
		as "go to declaration of a macro without arguments is correctly handled"
	, goToDeclarationOfMacroWithArgsCorrectlyHandled
		as "go to declaration of a macro with arguments is correctly handled"
	, goToDeclarationOfStdEnvFuncWhenLibraryIsPartOfConfig
		as "go to declaration of function defined in StdEnv when the StdEnv library is included in Eastwood.yml"
	, goToDeclarationOfStdEnvFuncWhenLibraryIsMissingInConfig
		as "go to declaration of function defined in StdEnv when the StdEnv library is not included in Eastwood.yml"
	, goToDeclarationOfTypeSynonymCorrectlyHandled
		as "go to declaration of a type synonym is correctly handled"
	, goToDeclarationOfAbstractTypeSynonymCorrectlyHandled
		as "go to declaration of an abstract type synonym is correctly handled"
	, goToDeclarationOfNewTypeCorrectlyHandled
		as "go to declaration of a newtype is correctly handled"
	, goToDeclarationOfAbstractNewTypeCorrectlyHandled
		as "go to declaration of an abstract newtype is correctly handled"
	, goToDeclarationOfRecordTypeInTypeSpecificationForRecordFieldCorrectlyHandled
		as "go to declaration of a record field access type specification is correctly handled"
	, goToDeclarationOfTypeDoesNotFindNewTypeWhenNameOfTypeIsInfixOfButDoesNotEqualNewTypeName
		as "go to declaration of a type does not find a newtype when the type name is infix of the newtype name."
	, goToDeclarationOfTypeInModuleWithNameForWhichUrlEncodingAltersName
		as "go to declaration of a type located within a module that has a name for which url encoding alters the name."
	, goToDeclarationOfUniqueTypeCorrectlyHandled
		as "go to declaration of a unique type is correctly handled"
	, goToDefinitionOfRecordFieldPrecededByBraceOnPreviousLineCorrectlyHandled
		as "go to definition of record field preceded by { on the previous line is correctly handled"
	, goToDefinitionOfRecordFieldPrecededByCommaOnPreviousLineCorrectlyHandled
		as "go to definition of record field preceded by , on the previous line is correctly handled"
	, goToDefinitionOfRecordFieldPrecededByCommaOnSameLineCorrectlyHandled
		as "go to definition of record field preceded by , on the same line is correctly handled"
	, goToDefinitionOfRecordFieldPrecededByBraceAndRecordDefinitionOnSameLineCorrectlyHandled
		as "go to definition of record field preceded by { and record definition on the same line is correctly handled"
	, goToDefinitionOfRecordFieldPrecededByCommaAndRecordDefinitionOnSameLineCorrectlyHandled
		as "go to definition of record field preceded by , and record definition on the same line is correctly handled"
	// There is a still an edge case here with lets say [(a,b):gFunc{|*|}] unless a space is added before gFunc.
	, goToDefinitionStopParsingSymbolsOfPrefixFunctionWhenSearchTermDoesNotContainGenericKindSpecificationCorrectlyRemoved
		as "go to definition stop parsing symbols when function is prefix and does not contain generic kind specification is correctly handled"
	, goToDefinitionOfTypeAlsoDefinedInOtherIclCorrectlyHandled
		as "go to definition of type which is also defined module local in another .icl module is correctly handled"
	, goToDefinitionOfGenericCorrectlyHandled
		as "go to definition of a generic is correctly handled"
	, goToDefinitionOfNewtypeCorrectlyHandled
		as "go to definition of a newtype is correctly handled"
	, goToDefinitionOfMacroCorrectlyHandled
		as "go to definition of a macro is correctly handled"
	, goToDefinitionOfTypeSynonymCorrectlyHandled
		as "go to definition of a type synonym is correctly handled"
	, goToDefinitionOfClassCorrectlyHandled
		as "go to definition of a class is correctly handled"
	, goToDefinitionOfClassSingleFunctionSyntaxCorrectlyHandled
		as "go to definition of a class which has one function and uses special syntax for this reason is correctly handled"
	, goToDefinitionOfConstructorPrecededByTypeDefCorrectlyHandled
		as "go to definition of a constructor which is preceded by a type definition on the same line is correctly handled"
	, goToDefinitionOfConstructorPrecededByTypeDefAndOtherConstructorCorrectlyHandled
		as "go to definition of a constructor which is a preceded by a type definition and another constructor on the same line is correctly handled"
	, goToDefinitionOfConstructorPrecededByTypeDefAndPipeOnPreviousLineCorrectlyHandled
		as "go to definition of a constructor which is preceded by a type def and pipe on the previous line is correctly handled"
	, goToDefinitionOfConstructorPrecededByPipeOnPreviousLineCorrectlyHandled
		as "go to definition of a constructor preceded by pipe on the previous line is correctly handled"
	, goToDefinitionOfConstructorPrecededByPipeOnSameLineCorrectlyHandled
		as "go to definition of a constructor preceded by pipe on the same line is correctly handled"
	, goToDefinitionOfConstructorWithArgsPrecededByPipeOnSameLineCorrectlyHandled
		as "go to definition of a constructor with args that is preceded by a pipe on the same line is correctly handled"
	, goToDefinitionOfConstructorWithArgsPrecededByPipeAndOtherConstructorOnSameLineCorrectlyHandled
		as "go to definition of a constructor with args that is preceded by a pipe and another constructor on the same line is correctly handled"
	, goToDefinitionOfConstructorPrecededByEqualsOnPreviousLineCorrectlyHandled
		as "go to definition of a constructor that is preceded by = (no type def) on the previous line is correctly handled"
	, goToDefinitionOfConstructorPrecededByEqualsOnSameLineCorrectlyHandled
		as "go to definition of a constructor that is preceded by = (no type def) on the same line is correctly handled"
	, goToDefinitionOfFuncNoTypeAnnotationEqualsSameLineCorrectlyHandled
		as "go to definition of a function without a type annotation with the = on the same line is correctly handled"
	, goToDefinitionOfFuncNoTypeAnnotationEqualsNextLineCorrectlyHandled
		as "go to definition of a function without a type annotation with the = on the next line is correctly handled"
	, goToDefinitionOfFuncNoTypeAnnotationLetNextLineCorrectlyHandled
		as "go to definition of a function without a type annotation with a # on the next line is correctly handled"
	, goToDefinitionOfFuncNoTypeAnnotationGuardNextLineCorrectlyHandled
		as "go to definition of a function without a type annotation with a | on the next line is correctly handled"
	// Copy of a different test but this test was added to describe the behavior of the current implementation.
	, goToDefinitionOfFuncWithTypeAnnotationResultsInTypeDefinitionLineOnly
		as "go to definition of a function with a type annotation only yields a result for the line containing the type definition"
	, goToDefinitionOfUniqueTypeCorrectlyHandled
		as "go to definition of a unique type is correctly handled"
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

goToDeclarationOfTypeSingleResultIsCorrectlyHandled :: Property
goToDeclarationOfTypeSingleResultIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		typeSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 6)!]
where
	typeSingleResultPosition :: Position
	typeSingleResultPosition = {line=uint 6, character=uint 6}

goToDeclarationOfTypeMultipleResultsIsCorrectlyHandled :: Property
goToDeclarationOfTypeMultipleResultsIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		typeMultipleResultsPosition
		[!(FILE_GO_TO_DCL_1, uint 4), (FILE_GO_TO_DCL_2, uint 4)!]
where
	typeMultipleResultsPosition :: Position
	typeMultipleResultsPosition = {line=uint 4, character=uint 12}

goToDeclarationOfFuncSingleResultIsCorrectlyHandled :: Property
goToDeclarationOfFuncSingleResultIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		funcSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 8)!]
where
	funcSingleResultPosition :: Position
	funcSingleResultPosition = {line=uint 8, character=uint 5}

goToDeclarationOfFuncMultipleResultsIsCorrectlyHandled :: Property
goToDeclarationOfFuncMultipleResultsIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_2
		funcMultipleResultsPosition
		[!(FILE_GO_TO_DCL_1, uint 10), (FILE_GO_TO_DCL_2, uint 6)!]
where
	funcMultipleResultsPosition :: Position
	funcMultipleResultsPosition = {line=uint 6, character=uint 5}

goToDeclarationOfInfixlFuncSingleResultIsCorrectlyHandled :: Property
goToDeclarationOfInfixlFuncSingleResultIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		infixFuncSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 35)!]
where
	infixFuncSingleResultPosition :: Position
	infixFuncSingleResultPosition = {line=uint 9, character = uint 56}

goToDeclarationOfInfixrFuncSingleResultIsCorrectlyHandled :: Property
goToDeclarationOfInfixrFuncSingleResultIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		infixrFuncSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 39)!]
where
	infixrFuncSingleResultPosition :: Position
	infixrFuncSingleResultPosition = {line=uint 3, character = uint 36}

goToDeclarationOfInfixFuncSingleResultIsCorrectlyHandled :: Property
goToDeclarationOfInfixFuncSingleResultIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		infixFuncSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 41)!]
where
	infixFuncSingleResultPosition :: Position
	infixFuncSingleResultPosition = {line=uint 3, character = uint 27}

goToDeclarationOfInfixlFuncMultipleResultsIsCorrectlyHandled :: Property
goToDeclarationOfInfixlFuncMultipleResultsIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		infixFuncMultipleResultsPosition
		[!(FILE_GO_TO_DCL_1, uint 37), (FILE_GO_TO_DCL_2, uint 19)!]
where
	infixFuncMultipleResultsPosition :: Position
	infixFuncMultipleResultsPosition = {line=uint 6, character = uint 41}

goToDeclarationOfInfixlFuncUsedPrefixSingleResultIsCorrectlyHandled :: Property
goToDeclarationOfInfixlFuncUsedPrefixSingleResultIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		infixFuncSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 35)!]
where
	infixFuncSingleResultPosition :: Position
	infixFuncSingleResultPosition = {line=uint 9, character = uint 41}

goToDeclarationOfInfixlFuncUsedPrefixMultipleResultsIsCorrectlyHandled :: Property
goToDeclarationOfInfixlFuncUsedPrefixMultipleResultsIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		infixFuncMultipleResultsPosition
		[!(FILE_GO_TO_DCL_1, uint 37), (FILE_GO_TO_DCL_2, uint 19)!]
where
	infixFuncMultipleResultsPosition :: Position
	infixFuncMultipleResultsPosition = {line=uint 6, character = uint 25}

goToDeclarationOfFuncThatStartsWithUppercaseIsCorrectlyHandled :: Property
goToDeclarationOfFuncThatStartsWithUppercaseIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		funcThatStartsWithUppercasePosition
		[!(FILE_GO_TO_DCL_1, uint 12)!]
where
	funcThatStartsWithUppercasePosition :: Position
	funcThatStartsWithUppercasePosition = {line=uint 12, character=uint 6}

goToDeclarationOfDeriveGenericFuncSingleResultIsCorrectlyHandled :: Property
goToDeclarationOfDeriveGenericFuncSingleResultIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		genericDeriveSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 14)!]
where
	genericDeriveSingleResultPosition :: Position
	genericDeriveSingleResultPosition = {line=uint 14, character=uint 12}

goToDeclarationOfDeriveGenericFuncMultipleResultsIsCorrectlyHandled :: Property
goToDeclarationOfDeriveGenericFuncMultipleResultsIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_2
		genericDeriveMultipleResultsPosition
		[!(FILE_GO_TO_DCL_1, uint 16), (FILE_GO_TO_DCL_2, uint 8)!]
where
	genericDeriveMultipleResultsPosition :: Position
	genericDeriveMultipleResultsPosition = {line=uint 8, character=uint 12}

goToDeclarationOfUsageOfMonoKindedGenericFuncSelectingKindSpecificationIsCorrectlyHandled :: Property
goToDeclarationOfUsageOfMonoKindedGenericFuncSelectingKindSpecificationIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		genericFuncUsagePosition
		[!(FILE_GO_TO_DCL_1, uint 59)!]
where
	genericFuncUsagePosition :: Position
	genericFuncUsagePosition = {line=uint 26, character=uint 33}

goToDeclarationOfUsageOfMonoKindedGenericFuncSelectingSpecialSyntaxSymbolOfKindSpecificationIsCorrectlyHandled
	:: Property
goToDeclarationOfUsageOfMonoKindedGenericFuncSelectingSpecialSyntaxSymbolOfKindSpecificationIsCorrectlyHandled
	=:	goToTest
			Declaration
			SUITE_DEFAULT
			FILE_GO_TO_ICL_1
			genericFuncUsagePosition
			[!(FILE_GO_TO_DCL_1, uint 59)!]
where
	genericFuncUsagePosition :: Position
	genericFuncUsagePosition = {line=uint 26, character=uint 34}

goToDeclarationOfUsageOfHigherKindedGenericFuncSelectingKindSpecificationIsCorrectlyHandled :: Property
goToDeclarationOfUsageOfHigherKindedGenericFuncSelectingKindSpecificationIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		genericFuncUsagePosition
		[!(FILE_GO_TO_DCL_1, uint 59)!]
where
	genericFuncUsagePosition :: Position
	genericFuncUsagePosition = {line=uint 31, character=uint 49}

goToDeclarationOfUsageOfHigherKindedGenericFuncSelectingSpecialSyntaxSymbolWithinKindSpecificationIsCorrectlyHandled
	:: Property
goToDeclarationOfUsageOfHigherKindedGenericFuncSelectingSpecialSyntaxSymbolWithinKindSpecificationIsCorrectlyHandled
	=:	goToTest
			Declaration
			SUITE_DEFAULT
			FILE_GO_TO_ICL_1
			genericFuncUsagePosition
			[!(FILE_GO_TO_DCL_1, uint 59)!]
where
	genericFuncUsagePosition :: Position
	genericFuncUsagePosition = {line=uint 31, character=uint 50}


goToDeclarationOfRecordFieldSingleResultIsCorrectlyHandled :: Property
goToDeclarationOfRecordFieldSingleResultIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		recordFieldSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 19)!]
where
	recordFieldSingleResultPosition :: Position
	recordFieldSingleResultPosition = {line=uint 19, character=uint 6}

goToDeclarationOfRecordFieldMultipleResultsIsCorrectlyHandled :: Property
goToDeclarationOfRecordFieldMultipleResultsIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		recordFieldMultipleResultsPosition
		[!(FILE_GO_TO_DCL_1, uint 25),(FILE_GO_TO_DCL_2, uint 12)!]
where
	recordFieldMultipleResultsPosition :: Position
	recordFieldMultipleResultsPosition = {line=uint 25, character=uint 6}

goToDeclarationOfClassFuncSingleResultIsCorrectlyHandled :: Property
goToDeclarationOfClassFuncSingleResultIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		classFuncSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 29)!]
where
	classFuncSingleResultPosition :: Position
	classFuncSingleResultPosition = {line=uint 29, character = uint 6}

goToDeclarationOfClassFuncMultipleResultsIsCorrectlyHandled :: Property
goToDeclarationOfClassFuncMultipleResultsIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_2
		classFuncMultipleResultsPosition
		[!(FILE_GO_TO_DCL_1, uint 32), (FILE_GO_TO_DCL_2, uint 16)!]
where
	classFuncMultipleResultsPosition :: Position
	classFuncMultipleResultsPosition = {line=uint 16, character = uint 6}

goToDeclarationOfClassSingleResultIsCorrectlyHandled :: Property
goToDeclarationOfClassSingleResultIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		classSingleResultPosition
		[!(FILE_GO_TO_DCL_1, uint 28)!]
where
	classSingleResultPosition :: Position
	classSingleResultPosition = {line=uint 28, character=uint 12}

goToDeclarationOfClassMultipleResultsIsCorrectlyHandled :: Property
goToDeclarationOfClassMultipleResultsIsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_2
		classMultipleResultsPosition
		[!(FILE_GO_TO_DCL_1, uint 31),(FILE_GO_TO_DCL_2, uint 15)!]
where
	classMultipleResultsPosition :: Position
	classMultipleResultsPosition = {line=uint 15, character=uint 12}

goToDeclarationOfFirstConstructorSameLineAsTypeDefCorrectlyHandled :: Property
goToDeclarationOfFirstConstructorSameLineAsTypeDefCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		constructorPosition
		[!(FILE_GO_TO_DCL_1, uint 43)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 43, character=uint 39}

goToDeclarationOfSecondConstructorSameLineAsTypeDefCorrectlyHandled :: Property
goToDeclarationOfSecondConstructorSameLineAsTypeDefCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		constructorPosition
		[!(FILE_GO_TO_DCL_1, uint 43)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 43, character=uint 47}

goToDeclarationOfConstructorPrecededByPipeOnPreviousLineWithTypeDefCorrectlyHandled :: Property
goToDeclarationOfConstructorPrecededByPipeOnPreviousLineWithTypeDefCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		constructorPosition
		[!(FILE_GO_TO_DCL_1, uint 44)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 44, character=uint 7}

goToDeclarationOfConstructorPrecededByPipeOnPreviousLineWithoutTypeDefCorrectlyHandled :: Property
goToDeclarationOfConstructorPrecededByPipeOnPreviousLineWithoutTypeDefCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		constructorPosition
		[!(FILE_GO_TO_DCL_1, uint 45)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 45, character=uint 7}

goToDeclarationOfConstructorPrecededByPipeOnSameLineWithoutTypeDefCorrectlyHandled :: Property
goToDeclarationOfConstructorPrecededByPipeOnSameLineWithoutTypeDefCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		constructorPosition
		[!(FILE_GO_TO_DCL_1, uint 46)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 46, character=uint 7}

goToDeclarationOfConstructorWithArgsPrecededByPipeOnSameLineWithoutTypeDefCorrectlyHandled :: Property
goToDeclarationOfConstructorWithArgsPrecededByPipeOnSameLineWithoutTypeDefCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		constructorPosition
		[!(FILE_GO_TO_DCL_1, uint 47)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 47, character=uint 7}

goToDeclarationOfConstructorWithArgsPrecededByOtherConstructorOnSameLineWithoutTypeDefCorrectlyHandled :: Property
goToDeclarationOfConstructorWithArgsPrecededByOtherConstructorOnSameLineWithoutTypeDefCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		constructorPosition
		[!(FILE_GO_TO_DCL_1, uint 47)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 47, character=uint 30}

goToDeclarationOfConstructorPrecededByEqualsOnSameLineWithoutTypeDefCorrectlyHandled :: Property
goToDeclarationOfConstructorPrecededByEqualsOnSameLineWithoutTypeDefCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		constructorPosition
		[!(FILE_GO_TO_DCL_1, uint 50)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 50, character=uint 10}

goToDeclarationOfConstructorPrecededByEqualsOnPreviousLineWithTypeDefCorrectlyHandled :: Property
goToDeclarationOfConstructorPrecededByEqualsOnPreviousLineWithTypeDefCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		constructorPosition
		[!(FILE_GO_TO_DCL_1, uint 53)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 53, character=uint 7}

goToDeclarationOfMacroWithArgsCorrectlyHandled :: Property
goToDeclarationOfMacroWithArgsCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		macroPosition
		[!(FILE_GO_TO_DCL_1, uint 81)!]
where
	macroPosition :: Position
	macroPosition = {line=uint 81, character=uint 7}

goToDeclarationOfMacroWithoutArgsCorrectlyHandled :: Property
goToDeclarationOfMacroWithoutArgsCorrectlyHandled =:
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

goToDeclarationOfTypeSynonymCorrectlyHandled :: Property
goToDeclarationOfTypeSynonymCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		typeSynonymPosition
		[!(FILE_GO_TO_DCL_1, uint 87)!]
where
	typeSynonymPosition :: Position
	typeSynonymPosition = {line=uint 87, character=uint 10}

goToDeclarationOfAbstractTypeSynonymCorrectlyHandled :: Property
goToDeclarationOfAbstractTypeSynonymCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		typeSynonymPosition
		[!(FILE_GO_TO_DCL_1, uint 88)!]
where
	typeSynonymPosition :: Position
	typeSynonymPosition = {line=uint 88, character=uint 10}

goToDeclarationOfNewTypeCorrectlyHandled :: Property
goToDeclarationOfNewTypeCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		newtypePosition
		[!(FILE_GO_TO_DCL_1, uint 90)!]
where
	newtypePosition :: Position
	newtypePosition = {line=uint 90, character=uint 8}

goToDeclarationOfAbstractNewTypeCorrectlyHandled :: Property
goToDeclarationOfAbstractNewTypeCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		newtypePosition
		[!(FILE_GO_TO_DCL_1, uint 91)!]
where
	newtypePosition :: Position
	newtypePosition = {line=uint 91, character=uint 8}

goToDeclarationOfRecordTypeInTypeSpecificationForRecordFieldCorrectlyHandled :: Property
goToDeclarationOfRecordTypeInTypeSpecificationForRecordFieldCorrectlyHandled =:
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

goToDeclarationOfUniqueTypeCorrectlyHandled :: Property
goToDeclarationOfUniqueTypeCorrectlyHandled =:
	goToTest
		Declaration
		SUITE_DEFAULT
		FILE_GO_TO_DCL_1
		uniqueTypePosition
		[!(FILE_GO_TO_DCL_1, uint 97)!]
where
	uniqueTypePosition :: Position
	uniqueTypePosition = {line=uint 97, character=uint 10}

goToDefinitionOfRecordFieldPrecededByBraceOnPreviousLineCorrectlyHandled :: Property
goToDefinitionOfRecordFieldPrecededByBraceOnPreviousLineCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		recordFieldPosition
		[!(FILE_GO_TO_ICL_1, uint 71)!]
where
	recordFieldPosition :: Position
	recordFieldPosition = {line=uint 71, character=uint 13}

goToDefinitionOfRecordFieldPrecededByCommaOnPreviousLineCorrectlyHandled :: Property
goToDefinitionOfRecordFieldPrecededByCommaOnPreviousLineCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		recordFieldPosition
		[!(FILE_GO_TO_ICL_1, uint 72)!]
where
	recordFieldPosition :: Position
	recordFieldPosition = {line=uint 72, character=uint 13}

goToDefinitionOfRecordFieldPrecededByCommaOnSameLineCorrectlyHandled :: Property
goToDefinitionOfRecordFieldPrecededByCommaOnSameLineCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		recordFieldPosition
		[!(FILE_GO_TO_ICL_1, uint 73)!]
where
	recordFieldPosition :: Position
	recordFieldPosition = {line=uint 73, character=uint 13}

goToDefinitionOfRecordFieldPrecededByBraceAndRecordDefinitionOnSameLineCorrectlyHandled :: Property
goToDefinitionOfRecordFieldPrecededByBraceAndRecordDefinitionOnSameLineCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		recordFieldPosition
		[!(FILE_GO_TO_ICL_1, uint 76)!]
where
	recordFieldPosition :: Position
	recordFieldPosition = {line=uint 76, character=uint 25}

goToDefinitionOfRecordFieldPrecededByCommaAndRecordDefinitionOnSameLineCorrectlyHandled :: Property
goToDefinitionOfRecordFieldPrecededByCommaAndRecordDefinitionOnSameLineCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		recordFieldPosition
		[!(FILE_GO_TO_ICL_1, uint 76)!]
where
	recordFieldPosition :: Position
	recordFieldPosition = {line=uint 76, character=uint 54}

goToDefinitionStopParsingSymbolsOfPrefixFunctionWhenSearchTermDoesNotContainGenericKindSpecificationCorrectlyRemoved
	:: Property
goToDefinitionStopParsingSymbolsOfPrefixFunctionWhenSearchTermDoesNotContainGenericKindSpecificationCorrectlyRemoved
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

goToDefinitionOfTypeAlsoDefinedInOtherIclCorrectlyHandled :: Property
goToDefinitionOfTypeAlsoDefinedInOtherIclCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		typePosition
		[!(FILE_GO_TO_ICL_1, uint 76), (FILE_GO_TO_ICL_2, uint 8)!]
where
	typePosition :: Position
	typePosition = {line=uint 76, character=uint 10}

goToDefinitionOfGenericCorrectlyHandled :: Property
goToDefinitionOfGenericCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		genericPosition
		[!(FILE_GO_TO_ICL_1, uint 34)!]
where
	genericPosition :: Position
	genericPosition = {line=uint 34, character=uint 11}

goToDefinitionOfNewtypeCorrectlyHandled :: Property
goToDefinitionOfNewtypeCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		newtypePosition
		[!(FILE_GO_TO_ICL_1, uint 82)!]
where
	newtypePosition :: Position
	newtypePosition = {line=uint 82, character=uint 8}

goToDefinitionOfMacroCorrectlyHandled :: Property
goToDefinitionOfMacroCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		macroPosition
		[!(FILE_GO_TO_ICL_1, uint 84)!]
where
	macroPosition :: Position
	macroPosition = {line=uint 84, character=uint 1}

goToDefinitionOfTypeSynonymCorrectlyHandled :: Property
goToDefinitionOfTypeSynonymCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		typeSynonymPosition
		[!(FILE_GO_TO_ICL_1, uint 86)!]
where
	typeSynonymPosition :: Position
	typeSynonymPosition = {line=uint 86, character=uint 8}

goToDefinitionOfClassCorrectlyHandled :: Property
goToDefinitionOfClassCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		classPosition
		[!(FILE_GO_TO_ICL_1, uint 88)!]
where
	classPosition :: Position
	classPosition = {line=uint 88, character=uint 11}

goToDefinitionOfClassSingleFunctionSyntaxCorrectlyHandled :: Property
goToDefinitionOfClassSingleFunctionSyntaxCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		classPosition
		[!(FILE_GO_TO_ICL_1, uint 91)!]
where
	classPosition :: Position
	classPosition = {line=uint 91, character=uint 11}

goToDefinitionOfConstructorPrecededByTypeDefCorrectlyHandled :: Property
goToDefinitionOfConstructorPrecededByTypeDefCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		constructorPosition
		[!(FILE_GO_TO_ICL_1, uint 93)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 93, character=uint 34}

goToDefinitionOfConstructorPrecededByTypeDefAndOtherConstructorCorrectlyHandled :: Property
goToDefinitionOfConstructorPrecededByTypeDefAndOtherConstructorCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		constructorPosition
		[!(FILE_GO_TO_ICL_1, uint 93)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 93, character=uint 47}

goToDefinitionOfConstructorPrecededByTypeDefAndPipeOnPreviousLineCorrectlyHandled :: Property
goToDefinitionOfConstructorPrecededByTypeDefAndPipeOnPreviousLineCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		constructorPosition
		[!(FILE_GO_TO_ICL_1, uint 94)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 94, character=uint 8}

goToDefinitionOfConstructorPrecededByPipeOnPreviousLineCorrectlyHandled :: Property
goToDefinitionOfConstructorPrecededByPipeOnPreviousLineCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		constructorPosition
		[!(FILE_GO_TO_ICL_1, uint 95)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 95, character=uint 8}

goToDefinitionOfConstructorPrecededByPipeOnSameLineCorrectlyHandled :: Property
goToDefinitionOfConstructorPrecededByPipeOnSameLineCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		constructorPosition
		[!(FILE_GO_TO_ICL_1, uint 96)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 96, character=uint 8}

goToDefinitionOfConstructorWithArgsPrecededByPipeOnSameLineCorrectlyHandled :: Property
goToDefinitionOfConstructorWithArgsPrecededByPipeOnSameLineCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		constructorPosition
		[!(FILE_GO_TO_ICL_1, uint 97)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 97, character=uint 8}

goToDefinitionOfConstructorWithArgsPrecededByPipeAndOtherConstructorOnSameLineCorrectlyHandled :: Property
goToDefinitionOfConstructorWithArgsPrecededByPipeAndOtherConstructorOnSameLineCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		constructorPosition
		[!(FILE_GO_TO_ICL_1, uint 97)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 97, character=uint 36}

goToDefinitionOfConstructorPrecededByEqualsOnSameLineCorrectlyHandled :: Property
goToDefinitionOfConstructorPrecededByEqualsOnSameLineCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		constructorPosition
		[!(FILE_GO_TO_ICL_1, uint 100)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 100, character=uint 8}

goToDefinitionOfConstructorPrecededByEqualsOnPreviousLineCorrectlyHandled :: Property
goToDefinitionOfConstructorPrecededByEqualsOnPreviousLineCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		constructorPosition
		[!(FILE_GO_TO_ICL_1, uint 103)!]
where
	constructorPosition :: Position
	constructorPosition = {line=uint 103, character=uint 8}

goToDefinitionOfFuncNoTypeAnnotationEqualsSameLineCorrectlyHandled :: Property
goToDefinitionOfFuncNoTypeAnnotationEqualsSameLineCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		funcPosition
		[!(FILE_GO_TO_ICL_1, uint 108)!]
where
	funcPosition :: Position
	funcPosition = {line=uint 108, character=uint 5}

goToDefinitionOfFuncNoTypeAnnotationEqualsNextLineCorrectlyHandled :: Property
goToDefinitionOfFuncNoTypeAnnotationEqualsNextLineCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		funcPosition
		[!(FILE_GO_TO_ICL_1, uint 110)!]
where
	funcPosition :: Position
	funcPosition = {line=uint 110, character=uint 5}

goToDefinitionOfFuncNoTypeAnnotationLetNextLineCorrectlyHandled :: Property
goToDefinitionOfFuncNoTypeAnnotationLetNextLineCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		funcPosition
		[!(FILE_GO_TO_ICL_1, uint 113)!]
where
	funcPosition :: Position
	funcPosition = {line=uint 113, character=uint 5}

goToDefinitionOfFuncNoTypeAnnotationGuardNextLineCorrectlyHandled :: Property
goToDefinitionOfFuncNoTypeAnnotationGuardNextLineCorrectlyHandled =:
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

goToDefinitionOfUniqueTypeCorrectlyHandled :: Property
goToDefinitionOfUniqueTypeCorrectlyHandled =:
	goToTest
		Definition
		SUITE_DEFAULT
		FILE_GO_TO_ICL_1
		uniqueTypePosition
		[!(FILE_GO_TO_ICL_1, uint 121)!]
where
	uniqueTypePosition :: Position
	uniqueTypePosition = {line=uint 121, character=uint 10}