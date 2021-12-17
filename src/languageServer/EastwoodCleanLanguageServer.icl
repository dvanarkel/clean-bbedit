module EastwoodCleanLanguageServer

import StdEnv
import StdOverloadedList

from Data.Error import fromOk, isError, :: MaybeError (Ok), fromError

import qualified Data.Error
import qualified Data.Map
import Data.Func
import Data.Either
import Data.Functor
import Data.List
import Data.Maybe
import Data.Tuple
import System.Environment
import System.File
import System.FilePath
import System.OS
from System.Process import :: ProcessResult {..}, callProcessWithOutput
import Text
import Text.GenJSON
import Text.URI
import Text.YAML
from Text.Unicode.UChar import isSymbol, :: UChar, instance fromChar UChar, instance toChar UChar, instance == UChar
	, instance toInt UChar, isAlphaNum, isPunctuation

from LSP.Diagnostic import qualified :: Diagnostic {..}, :: DiagnosticSeverity {..}
from LSP.Position import qualified :: Position {..}
from LSP.PublishDiagnosticsParams import qualified :: PublishDiagnosticsParams {..}
from LSP.Range import qualified :: Range {..}
import LSP
import LSP.BasicTypes
from LSP.DidOpenTextDocumentParams import qualified :: DidOpenTextDocumentParams {..}
from LSP.DidSaveTextDocumentParams import qualified :: DidSaveTextDocumentParams {..}
import LSP.InitializeParams
import LSP.Internal.Serialize
from LSP.MessageParams import :: MessageParams (..), :: MessageType (..)
import LSP.ShowMessageParams
import LSP.NotificationMessage
import LSP.RequestMessage
import LSP.ResponseMessage
import LSP.ServerCapabilities
import LSP.TextDocumentIdentifier
from LSP.Location import :: Location (..)
import qualified LSP.Position
import qualified LSP.PublishDiagnosticsParams
import qualified LSP.Range

from Eastwood.Diagnostic import :: DiagnosticSource
from Eastwood.Diagnostic import qualified :: Diagnostic {..}, :: DiagnosticSeverity
from Eastwood.Range import :: Range {..}, :: CharacterRange
from Eastwood.Range import qualified :: Position {..}
import qualified Eastwood.Diagnostic
import qualified Eastwood.Range
import Eastwood.Util.FileFinder
import Eastwood.Util.ModuleNameResolver

import Compiler

CLEAN_HOME_ENV_VAR :== "CLEAN_HOME"
EXE_PATH :== "lib/exe"
LIBS_PATH :== "lib"

PROJECT_FILENAME :== "Eastwood.yml"
README_LINK :== "https://gitlab.com/top-software/eastwood/-/blob/main/README.md"

Start :: !*World -> *World
Start w = serve capabilities cleanLanguageServer w

capabilities :: ServerCapabilities
capabilities =
	{ ServerCapabilities
	| textDocumentSync = {openClose = True, save = True}
	, declarationProvider = True
	}

:: EastwoodState = {workspaceFolders :: ![!FilePath]}

//* Shadow for `CompilerSettings` on which `gConstructFromYAML` can be derived.
:: CompilerSettingsConfig =
	{ compiler :: !FilePath
		//* Compiler's executable name (e.g. `cocl`, `cocl-itasks`), supposed
		//* to be found in `CLEAN_HOME/EXE_PATH`.
	, libraries :: ![String]
		//* Library names, found in `CLEAN_HOME/LIB_PATH`.
	, paths :: ![FilePath]
		//* Extra search paths, either absolute or relative to
		//* `PROJECT_FILENAME`.
	}

derive gConstructFromYAML CompilerSettingsConfig

cleanLanguageServer :: LanguageServer EastwoodState
cleanLanguageServer = {onInitialize = onInitialize, onRequest = onRequest, onNotification = onNotification}

onInitialize :: !InitializeParams !*World -> (!MaybeError String EastwoodState, !*World)
onInitialize {rootPath, rootUri, workspaceFolders} world
	= ('Data.Error'.Ok {EastwoodState|workspaceFolders=workspace}, world)
where
	workspace :: [!FilePath]
	workspace = case workspaceFolders of
		?Just folders -> [|uriToString uri \\ {WorkspaceFolder | uri} <|- folders]
		?None -> case rootUri of
			?Just uri -> [|uriToString uri]
			?None -> case rootPath of
				?Just path -> [|path]
				?None -> [|]
	where
		// Strip the file:// prefix; this is needed for `readFile` etc. to work
		uriToString uri = toString {uri & uriScheme = ?None}

fetchConfig :: ![!FilePath] !*World -> (!'Data.Error'.MaybeError String CompilerSettings, !*World)
fetchConfig workspaceFolders world
	# (mbConfigPath, world) = findSearchPath PROJECT_FILENAME workspaceFolders world
	| isNone mbConfigPath
		= ('Data.Error'.Error $
			concat
				[ "Could not find the "
				, PROJECT_FILENAME
				, " project configuration file in the workspace folder. Please create the file in the workspace's root folder. The expected format of the "
				, PROJECT_FILENAME
				, " file is described in "
				, README_LINK
				, "."
				]
			, world)
	# configPath = fromJust mbConfigPath </> PROJECT_FILENAME
	# (mbConfig, world) = readFile configPath world
	// Check if the project file could be read.
	| 'Data.Error'.isError mbConfig =
		('Data.Error'.Error $
			concat4 "Cannot read project file found at " configPath ": " (toString $ 'Data.Error'.fromError mbConfig)
		, world)
	# config = 'Data.Error'.fromOk mbConfig
	// Parse the YAML, ignore warnings
	# mbYML = loadYAML coreSchema config
	// Check if the YML could be parsed.
	| 'Data.Error'.isError mbYML
		=
		( 'Data.Error'.Error $
			concat
				[ "Invalid format of project file "
				, configPath
				, ": "
				, (toString $ 'Data.Error'.fromError mbYML)
				, ". The expected format of the project file is described in "
				, README_LINK
				]
		, world)
	# config = fst $ 'Data.Error'.fromOk mbYML
	// Interpret the paths relative to the path of the configuration file.
	// The config file path is included by default, so that a developer doesn't have to explicitly add `.` in the config
	// file to include the root directory.
	# config & paths = [takeDirectory configPath : [takeDirectory configPath </> p \\ p <- config.paths]]
	// Get CLEAN_HOME
	# (mbCleanHome, world) = getEnvironmentVariable CLEAN_HOME_ENV_VAR world
	  cleanHome = fromJust mbCleanHome
	| isNone mbCleanHome =
		( 'Data.Error'.Error $ concat3 "Could not get " CLEAN_HOME_ENV_VAR " environment variable", world)
	# searchPaths = (libPathFor cleanHome <$> config.libraries) ++ config.paths
	# (fullSearchPaths, world) = mapSt getFullPathName searchPaths world
	# mbErr = firstSearchPathError searchPaths fullSearchPaths
	| isJust mbErr = ('Data.Error'.Error $ fromJust mbErr, world)
	# config =
		{ compilerPath = cleanHome </> EXE_PATH </> config.compiler
		, searchPaths = map 'Data.Error'.fromOk fullSearchPaths
		}
	= ('Data.Error'.Ok config, world)
where
	libPathFor :: !FilePath !FilePath -> FilePath
	libPathFor cleanHome lib = cleanHome </> LIBS_PATH </> lib

	firstSearchPathError :: ![FilePath] ![MaybeOSError FilePath] -> ?String
	firstSearchPathError paths fullPaths = case filter ('Data.Error'.isError o snd) $ zip2 paths fullPaths of
		[] -> ?None
		[(path, 'Data.Error'.Error (_, error)):_] -> ?Just $ concat
			[ "Failed to find full path of ", path
			, " mentioned in ", PROJECT_FILENAME
			, ": ", error
			]

onRequest :: !RequestMessage !EastwoodState !*World -> (!ResponseMessage, !EastwoodState, !*World)
onRequest msg=:{RequestMessage | id, method} st world =
	case method of
		"textDocument/declaration" = onGotoDeclaration msg st world
		_ = (errorResponse id, st, world)
where
	errorResponse :: !RequestId -> ResponseMessage
	errorResponse id =
		{ ResponseMessage
		| id = ?Just id
		, result = ?None
		, error = ?Just
			{ ResponseError
			| errorCode = InternalError
			, message = "This language server is still work in progress."
			, data = ?None
			}
		}

onNotification :: !NotificationMessage !EastwoodState !*World -> (![!NotificationMessage], !EastwoodState, !*World)
onNotification {NotificationMessage| method, params} st world
	| method == "textDocument/didSave" || method == "textDocument/didOpen"
		| isNone params
			= ([!errorLogMessage (concat3 "Missing argument for '" method "'.")], st, world)
		# (diags, world) = diagnosticsFor (textDocument (fromJust params)) st world
		= case diags of
			'Data.Error'.Ok (notifications, diags) =
				( notifications ++|
					[!notificationMessage "textDocument/publishDiagnostics" (?Just diag) \\ diag <|- diags]
				, st
				, world)
			'Data.Error'.Error err =
				([!errorLogMessage err], st, world)
		with
			textDocument params = case method of
				"textDocument/didSave" -> (deserialize params).'LSP.DidSaveTextDocumentParams'.textDocument
				"textDocument/didOpen" -> (deserialize params).'LSP.DidOpenTextDocumentParams'.textDocument
	| method == "textDocument/didClose"
		= ([!], st, world)
	| method == "$/setTrace"
		= ([!], st, world)
	| otherwise
		= ([!errorLogMessage $ concat3 "Unknown notification '" method "'."], st, world)

/**
 * Sends a response to the client containing the locations of the declarations which were requested by the client.
 * based on a request.
 */
onGotoDeclaration :: !RequestMessage !EastwoodState !*World -> (!ResponseMessage, !EastwoodState, !*World)
onGotoDeclaration req=:{RequestMessage|id, params=(?Just json)} st=:{EastwoodState|workspaceFolders} world
	# {GotoDeclarationParams|textDocument={TextDocumentIdentifier|uri}, position} = deserialize json
	# (mbLine, world) = getLineOfDeclarationRequest uri position world
	| isError mbLine = (fromError mbLine, st, world)
	# line = fromOk mbLine
	// NB: charNr does not mean column number, it is the number of the character within the line.
	// E.g tab = x cols 1 char.
	# charNr = position.'LSP.Position'.character
	// Parse the grep search term for which a declaration was requested.
	# mbSearchTerms = grepSearchTermFor line charNr
	| isError mbSearchTerms = (fromError mbSearchTerms, st, world)
	// Using the searchString, grep is executed to find the file names and line numbers of the definitions that match.
	// There is a special case constructors which have the preceding | or = on the preceding line.
	// This requires checking the previous line, hence there is a seperate search term for efficiency reasons.
	# (searchTermReg, searchTermConstructorSpecialCase)  = fromOk mbSearchTerms
	// The root path is necessary because otherwise grep returns relative paths and the client needs absolute paths.
	// The config path is the root path.
	# (mbConfigPath, world) = findSearchPath PROJECT_FILENAME workspaceFolders world
	| isNone mbConfigPath
		= (errorResponse id InternalError ("Could not find absolute path of " +++ PROJECT_FILENAME), st, world)
	# searchPath = fromJust mbConfigPath
	// We remove symlinks/../. to get the actual full path.
	# (mbRootPath, world) = getFullPathName searchPath world
	| isError mbRootPath =
		(errorResponse id InternalError ("Could not find absolute path of " +++ PROJECT_FILENAME), st, world)
	# rootPath = fromOk mbRootPath
	// CLEAN_HOME_ENV_VAR is also searched by grep so the value is retrieved to get the path.
	# (mbCleanHome, world) = getEnvironmentVariable CLEAN_HOME_ENV_VAR world
	| isNone mbCleanHome
		= (errorResponse id UnknownErrorCode "Could not find CLEAN_HOME environment variable.", st, world)
	# cleanHomePath = fromJust mbCleanHome
	// Call grep using the regular (non special constructor case) search term to find the matching declarations.
	// -P enables perl regexp, -r recurses through all files, -n gives line number,
	// -w matches whole words only (e.g: :: Maybe matches :: Maybe but not :: MaybeOSError).
	//--include \*.dcl makes sure only dcl files are examined by grep.
	# (mbGrepResultRegSearchTerm, world)
		= callProcessWithOutput
			"grep"
			["-P", searchTermReg, "-r", "-n", "-w", "--include", "\*.dcl", rootPath, cleanHomePath]
			?None
			world
	| isError mbGrepResultRegSearchTerm
		= (errorResponse id InternalError "grep failed when searching for type definitions.", st, world)
	# {stdout} = fromOk mbGrepResultRegSearchTerm
	# stdoutRegSearchTerm = stdout
	// The stdout for the special constructor case is processed using grep again to find the locations of the
	// Constructors which are preceded by | and = on the preceding line.
	# (mbGrepResultSpecialConstructorCase, world) = case searchTermConstructorSpecialCase of
		?None = (?None, world)
		?Just searchTerm =
			appFst ?Just $
				callProcessWithOutput
					"grep"
					[ "-P", searchTerm
					// -B 1 also selects the previous line.
					 , "-B", "1"
					 , "-r"
					 , "-n"
					 , "-w"
					 , "--include", "\*.dcl"
					 , rootPath
					 , cleanHomePath
					 ]
					?None
					world
	| isJust mbGrepResultSpecialConstructorCase && (isError $ fromJust mbGrepResultSpecialConstructorCase)
		= (errorResponse id InternalError "grep failed when searching for special constructor case.", st, world)
	# stdoutSpecialConstructorCase = case mbGrepResultSpecialConstructorCase of
		// No need to search for a constructor since the searchTerm can not be a constructor.
		?None = ""
		// There is a need to search for a constructor since the searchTerm could be a constructor.
		?Just grepResult = (fromOk grepResult).ProcessResult.stdout
	// List of lists of string containing the results, grep separates file name/line number by : and results by \n.
	// grep adds an empty newline at the end of the results which is removed by init.
	// The first two results are the file name and line number.
	// This is transformed into a list of tuples of filename and linenumber for convenience.
	// ?None and ?Just are currently exceptions.
	# results =
		(	(\[fileName, lineNr] -> ([(fileName, toInt lineNr)])) o take 2 o split ":" <$>
				(init $ split "\n" stdoutRegSearchTerm)
		)
		++
		// - is used as a seperator for the specialConstructorCase results, as grep uses - as a group seperator.
		// when previous lines are shown instead of :.
		// In the special constructor case the previous line will be found so +1 is added to the line number.
		(	(\[fileName, lineNr] -> ([(fileName, toInt lineNr + 1)])) o take 2 o split "-" <$>
				(filterConstuctorCaseResults $ init $ split "\n" stdoutSpecialConstructorCase)
		)
	// For every tuple of fileName and lineNumber, a Location is generated to be sent back to the client.
	# locations = [! l \\ l <- catMaybes $ fileAndLineToLocation <$> flatten results !]
	= (locationResponse id locations, st, world)
where
	/**
	 * This function returns the line for which a go to declaration request was made in string form.
	 *
	 * @param The URI that is provided by the client, indicates file for which request was made.
	 * @param The position within the file for which the request was made
	 * @param World
	 * @result an error response to send back to the client in case of failure or the line of the request as a string.
	 * @result World
	 */
	getLineOfDeclarationRequest :: !URI 'LSP.Position'.Position !*World -> (!MaybeError ResponseMessage String, !*World)
	getLineOfDeclarationRequest uri position world
		# (mbLines, world) = readFileLines uri.uriPath world
		| isError mbLines =
			('Data.Error'.Error $
				errorResponse id ContentModified (concat3 "The file located at " uri.uriPath "was not found.")
			, world
			)
		# lines = fromOk mbLines
		# (UInt lineNr) = position.'LSP.Position'.line
		# mbLine = lines !? lineNr
		| isNone mbLine =
			('Data.Error'.Error $
				errorResponse
					id
					ParseError
					(concat4 "The file located at " uri.uriPath "does no longer contain line number " (toString lineNr))
			, world
			)
		// The line was found.
		= (Ok $ fromJust mbLine, world)

	/**
	 * This function retrieves the search term that is passed to grep which is used for finding the declaration.
	 * If the function succeeds, two search terms (String, ?String) are returned.
	 * The first one (String) is the general search term.
	 * The second search term (?String) is for constructors where the preceding = or | is on the previous line.
	 * If the second search term is ?None, the search term can not possibly be a constructor.
	 * grep only handles one line at a time so therefore we need to process this case differently.
	 *
	 * @param The line number for which a declaration was requested.
	 * @param The character number that was selected when a declaration request was made.
	 * @result an error response to be sent back to the client or the search terms used by grep.
	 */
	grepSearchTermFor :: !String !UInt -> MaybeError ResponseMessage (String,?String)
	grepSearchTermFor line (UInt charNr)
		# firstUnicodeChar = fromChar $ select line charNr
		// If the first char is a space, comma, \n, or \t, go backwards
		// When a declaration is requested when a whole term is selected the character ends up being the first char
		// after the term, the same holds when attempting to go to the declaration when selecting a lookBackCharacter.
		# lookBackCharacters = fromChar <$> [' ', ',', '\n', '\t', ')']
		| elem firstUnicodeChar lookBackCharacters = grepSearchTermFor line (UInt (charNr - 1))
		// This case is added to deal with going to the declaration of an infix function that is used prefix
		// and selecting the ( character.
		# lookForwardCharacter = fromChar '('
		| firstUnicodeChar == lookForwardCharacter = grepSearchTermFor line (UInt (charNr + 1))
		// It should not be attempted to go the declaration of special syntax symbols.
		| isSpecialSymbol firstUnicodeChar =
			'Data.Error'.Error $
				errorResponse id ParseError "it is not possible to go to the definition of a special syntax symbol."
		// This is the general case.
		| isSymbol firstUnicodeChar || isAlphaNum firstUnicodeChar || isPunctuation firstUnicodeChar
			# stopPredicate =
				// The parser stops parsing based on a different condition if an infix function is parsed.
				if (isSymbol firstUnicodeChar || isPunctuation firstUnicodeChar)
					symbolPuncStopPredicate
					alphaNumStopPredicate
			// Parse backwards and forwards until a char that triggers the stopPredicate is found
			// or when we run out of chars to parse.
			# searchTerm =
				toString $
					(Reverse $ parseSearchTerm line stopPredicate (Reverse [!0..charNr-1!])) ++|
					parseSearchTerm line stopPredicate [!charNr..size line!]
			# atleastOneWhiteSpace = "(\\s+)"
			// The ^ indicates that the term that follows should not be preceded by any characters.
			// This is used to avoid finding imports as declarations terms are never preceded by characters.
			# avoidImports = "^"
			# startsWithUpper = isUpper $ select searchTerm 0
			// Types always start with a uppercase character.
			# grepTypeSearchTerm = if startsWithUpper (concat3 avoidImports ":: " searchTerm) ""
			// The grep func definition search pattern is adjusted based on
			// whether an infix function or a prefix function was parsed.
			# grepFuncSearchTerm =
				if (isSymbol firstUnicodeChar || isPunctuation firstUnicodeChar)
					(	let
							// Characters which should be escaped to avoid them being seen as regex..
							// See https://riptutorial.com/regex/example/15848/what-characters-need-to-be-escaped.
							charactersToEscape
								= ['[', ']', '(', ')', '{', '}', '*', '+', '?', '|', '^', '$', '.', '\\']
							// Every character that should be escaped results in 2 characters (one for the \)
							escapedSearchTerm
								= concat $
									[ if (elem c charactersToEscape) ("\\" +++ toString c) (toString c)
									  \\ c <-: searchTerm
									]
						// infix[lr]? indicates infix followed by l, r, or nothing.
						in concat5 "\\(" escapedSearchTerm "\\)" atleastOneWhiteSpace "infix[lr]?"
					)
					(concat3 searchTerm atleastOneWhiteSpace "::" )
			# grepGenericSearchTerm = concat4 avoidImports "generic" atleastOneWhiteSpace searchTerm
			# grepClassSearchTerm = concat4 avoidImports "class" atleastOneWhiteSpace searchTerm
			// For clarity we define this variable as a duplicate of avoidImports.
			# stringStartsWith = "^"
			# anyAmountOfCharacters = ".*"
			# atleastOneCharacter = ".+"
			# pipeOrEquals ="\\||="
			# grepConstructorSearchTerm
				// Constructors always start with a uppercase letter, so do not search if this is not the case.
				// The pipe or = preceding the constructor may be preceded by either
				// 1: :: followed by any combination of characters followed by | or = followed by
				// at least one whitespace followed by the search term
				// 2: At least one white space followed by any combination of characters followed by | or = followed by
				// at least one whitespace followed by the search term.
				= if startsWithUpper
					(concat
						[ stringStartsWith
						, "("
						, 	"("
						, 	atleastOneWhiteSpace
						, 	anyAmountOfCharacters
						, 	")"
						, 	"|" // OR.
						,   "::"
						,   atleastOneCharacter
						, ")"
						, "("
						, pipeOrEquals
						, ")"
						, atleastOneWhiteSpace
						, searchTerm
						]
					)
					""
			// Further processing has to be done for constructors that have the | or = on the previous line.
			// In this case, the constructor has to be preceded by at least one whitespace only.
			// For this we return a seperate search term since we have to process the previous line.
			# grepConstructorSearchTermSpecialCase =
				if (startsWithUpper) (?Just $ concat3 stringStartsWith atleastOneWhiteSpace searchTerm) ?None
			= Ok $
				(concat
					// Only search for types when the term starts with an uppercase character.
					[ if (grepTypeSearchTerm == "") "" (grepTypeSearchTerm +++ "|")
					, grepFuncSearchTerm
					, "|"
					, grepGenericSearchTerm
					, "|"
					, grepClassSearchTerm
					// Only search for constructors if the term starts with an uppercase character.
					, if (grepConstructorSearchTerm == "") "" ("|" +++ grepConstructorSearchTerm)
					]
				, grepConstructorSearchTermSpecialCase)
		= 'Data.Error'.Error $
			errorResponse
			id
			ParseError
			("Unrecognised char with unicode : " +++ (toString $ toInt firstUnicodeChar))
	where
		/**
		 * This function parses the raw search term for which a declaration was requested without adding regexp
		 * to filter results.
		 *
		 * @param The line on which a go to declaration was made.
		 * @param A predicate that defines when parsing should stop based on the unicode character that is being parsed.
		 * @param The character indices within the line that should be parsed.
		 * @result The parsed raw search term.
		 */
		parseSearchTerm :: !String !(UChar -> Bool) ![!Int!] -> [!Char!]
		parseSearchTerm line stopPredicate indexes = parseSearchTerm` line filter indexes [!!]
		where
			parseSearchTerm` line filter [!i:is!] acc
				# uChar = fromChar $ select line i
				// If there is a character that adheres to the stop filter, break out of the recursion.
				| stopPredicate uChar = Reverse acc
				// Parsed a character, go to the next index.
				= parseSearchTerm` line filter is [!toChar uChar:acc!]
			// If there are no indexes left, we return the accumlator of characters for which the filter holds.
			parseSearchTerm` _ _ [!!] acc = Reverse acc

		symbolPuncStopPredicate :: !UChar -> Bool
		symbolPuncStopPredicate uc
			= not (isSymbol uc || isPunctuation uc) || isSpecialSymbol uc || elem uc (fromChar <$> ['(', ')'])

		alphaNumStopPredicate :: !UChar -> Bool
		alphaNumStopPredicate uc = not (isAlphaNum uc)

		// Not possible to retrieve a declaration for these symbols are they are special syntax characters.
		// Not strict because there is currently no Elem in StdOverloadedList.
		specialSymbols :: [UChar]
		specialSymbols = map fromChar ['{', '}', '[', ']', ';', '\"', '\'', '_']

		isSpecialSymbol :: !UChar -> Bool
		isSpecialSymbol uc = elem uc specialSymbols

	//* Filename/line/match is separated by : for actual matches and - for previous lines.
	parseGrepResults :: !String !String -> [[String]]
	parseGrepResults sep stdoutH = split ":" <$> (init $ split "\n" stdoutH)

	filterConstuctorCaseResults :: ![String] -> [String]
	filterConstuctorCaseResults strs =
		[previousLine
		\\ previousLine
			<- filter
				(\str
					# lastHyphen = lastIndexOf "-" str
					// string has to contain no : before the last hyphen (since this means it is the actual match).
					// and not the previous line. There has to be a hyphen in the line since otherwise it can not
					// be a previous line to begin with.
					-> if (lastHyphen == -1) False (if (indexOf ":" str == -1) True (indexOf ":" str > lastHyphen))
				)
				strs
		| hasPipeOrEqualsAtEnd previousLine
		]
	where
		hasPipeOrEqualsAtEnd :: !String -> Bool
		hasPipeOrEqualsAtEnd str = hasOnlyWhiteSpaceBeforePipe $ reverse [c \\ c <-:str]
		where
			hasOnlyWhiteSpaceBeforePipe :: ![Char] -> Bool
			hasOnlyWhiteSpaceBeforePipe ['|':_] = True
			hasOnlyWhiteSpaceBeforePipe ['=':_] = True
			hasOnlyWhiteSpaceBeforePipe [' ':cs] = hasOnlyWhiteSpaceBeforePipe cs
			hasOnlyWhiteSpaceBeforePipe ['\t':cs] = hasOnlyWhiteSpaceBeforePipe cs
			hasOnlyWhiteSpaceBeforePipe _ = False

	errorResponse :: !RequestId !ErrorCode !String -> ResponseMessage
	errorResponse id err msg =
		{ ResponseMessage
		 | id = ?Just id
		, result = ?None
		, error = ?Just
			{ ResponseError
			| errorCode = err
			, message = msg
			, data = ?None
			}
		}

	locationResponse :: !RequestId ![!Location!] -> ResponseMessage
	locationResponse id locations=
		{ ResponseMessage
		| id = ?Just id
		, result =
			?Just $ serialize locations
		, error = ?None
		}

	fileAndLineToLocation :: !(!String, !Int) -> ?Location
	fileAndLineToLocation (filePath, lineNr)
		# fileUri = parseURI $ "file://" </> filePath
		| isNone fileUri = ?None
		= ?Just $
			{ Location
			| uri = fromJust fileUri
			, range = rangeCorrespondingTo
				{ 'Eastwood.Range'.Range
				| start={'Eastwood.Range'.Position|'Eastwood.Range'.line=lineNr-1, 'Eastwood.Range'.character=0}
				, end={'Eastwood.Range'.Position|'Eastwood.Range'.line=lineNr-1, 'Eastwood.Range'.character=0}
				}
			}

:: GotoDeclarationParams =
	{ textDocument :: !TextDocumentIdentifier
	, position :: !'LSP.Position'.Position
	}

derive gLSPJSONDecode GotoDeclarationParams

errorLogMessage :: !String -> NotificationMessage
errorLogMessage message = showMessage {MessageParams| type = Error, message = message}

diagnosticsFor ::
	!TextDocumentIdentifier !EastwoodState !*World
	-> (!MaybeError String ([!NotificationMessage], [!'LSP.PublishDiagnosticsParams'.PublishDiagnosticsParams]), !*World)
diagnosticsFor {TextDocumentIdentifier| uri = uri=:{uriPath}} {EastwoodState|workspaceFolders} world
	# (mbCompilerSettings, world) = fetchConfig workspaceFolders world
	| 'Data.Error'.isError mbCompilerSettings
		= ('Data.Error'.Ok $ ([!errorLogMessage $ 'Data.Error'.fromError mbCompilerSettings], [!]), world)
	# compilerSettings = fromOk mbCompilerSettings
	# (mbModuleName, world) = resolveModuleName uriPath compilerSettings.searchPaths world
	  moduleName = fromOk mbModuleName
	| 'Data.Error'.isError mbModuleName
		// Create a ad hoc diagnostic for failing to determine the module name:
		// we want to return an `Ok` so that we send diagnostics for this file,
		// instead of tracing an error message with `showMessage`.
		# diagnostic =
			{ 'LSP.PublishDiagnosticsParams'.uri = uri
			, 'LSP.PublishDiagnosticsParams'.diagnostics =
				[|{'LSP.Diagnostic'.range =
					{ 'LSP.Range'.start = {'LSP.Position'.line = uint 0, 'LSP.Position'.character = uint 0}
					, 'LSP.Range'.end = {'LSP.Position'.line = uint 0, 'LSP.Position'.character = uint 999999}
					}
				, 'LSP.Diagnostic'.severity = ?Just 'LSP.Diagnostic'.Error
				, 'LSP.Diagnostic'.codeDescription = ?None
				, 'LSP.Diagnostic'.source = ?None
				, 'LSP.Diagnostic'.message = "Failed to determine module name: " +++ 'Data.Error'.fromError mbModuleName
				, 'LSP.Diagnostic'.tags = [|]
				, 'LSP.Diagnostic'.relatedInformation = [|]
				, 'LSP.Diagnostic'.data = JSONNull
				}]
			}
		=
			( 'Data.Error'.Ok ([!], [!diagnostic])
			, world)
	# (diagnostics, world) = runCompiler uriPath moduleName compilerSettings world
	= case diagnostics of
		'Data.Error'.Ok diagnostics
			# (notifications, diagnostics, world) = collectDiagnostics compilerSettings diagnostics world
			-> ('Data.Error'.Ok (notifications, diagnostics), world)
		error
			-> ('Data.Error'.liftError error, world)
where
	collectDiagnostics
		:: !CompilerSettings !('Data.Map'.Map FilePath [!Diagnostic]) !*World
		-> (![!NotificationMessage], ![!'LSP.PublishDiagnosticsParams'.PublishDiagnosticsParams], !*World)
	collectDiagnostics compilerSettings diagnostics world
		= 'Data.Map'.foldrWithKey` append ([!], [!], world) diagnostics
	where
		append fileName diagnosticsForFile (notifications, collectedDiagnostics, world)
			// We need the fileName with `OS_PATH_SEPARATOR`s for
			// `findSearchPath`, but afterwards convert it to use `/` when
			// generating a URI.
			# (mbPath, world) = findSearchPath fileName compilerSettings.searchPaths world
			| isNone mbPath
				# notifications =
					[! showMessage {type = Warning, message = "Failed to find path for " +++ fileName}
					: notifications
					]
				= (notifications, collectedDiagnostics, world)
			# uriPath = fromJust mbPath </> {if (c == OS_PATH_SEPARATOR) '/' c \\ c <-: fileName}
			// Use getFullPathName to remove . and .. from paths (mainly to
			// ease testing because it gives a normalized form for paths)
			# (mbRealPath, world) = getFullPathName uriPath world
			# uriPath = fromMaybe uriPath $ 'Data.Error'.error2mb mbRealPath
			# collectedDiagnostics =
				[!
					{ 'LSP.PublishDiagnosticsParams'.uri = {uri & uriPath = uriPath}
					, 'LSP.PublishDiagnosticsParams'.diagnostics = Map lspDiagnosticFor diagnosticsForFile
					}
				: collectedDiagnostics
				]
			= (notifications, collectedDiagnostics, world)

	lspDiagnosticFor :: !'Eastwood.Diagnostic'.Diagnostic -> 'LSP.Diagnostic'.Diagnostic
	lspDiagnosticFor diagnostic =
		{ 'LSP.Diagnostic'.range = rangeCorrespondingTo diagnostic.'Eastwood.Diagnostic'.range
		, 'LSP.Diagnostic'.severity = ?Just $ severityCorrespondingTo diagnostic.'Eastwood.Diagnostic'.severity
		, 'LSP.Diagnostic'.codeDescription = ?None
		, 'LSP.Diagnostic'.source = ?None
		, 'LSP.Diagnostic'.message = diagnostic.'Eastwood.Diagnostic'.message
		, 'LSP.Diagnostic'.tags = [!]
		, 'LSP.Diagnostic'.relatedInformation = [!]
		, 'LSP.Diagnostic'.data = JSONNull
		}

rangeCorrespondingTo :: !('Eastwood.Range'.Range 'Eastwood.Range'.Position) -> 'LSP.Range'.Range
rangeCorrespondingTo range =
	{'LSP.Range'.Range
		| 'LSP.Range'.start = positionCorrespondingTo range.'Eastwood.Range'.Range.start
		, 'LSP.Range'.end = positionCorrespondingTo range.'Eastwood.Range'.Range.end
	}
where
	positionCorrespondingTo :: !'Eastwood.Range'.Position -> 'LSP.Position'.Position
	positionCorrespondingTo {'Eastwood.Range'.Position| 'Eastwood.Range'.line, 'Eastwood.Range'.character} =
		{'LSP.Position'.Position
			| 'LSP.Position'.line = uint line
			, 'LSP.Position'.character = uint character
		}

instance toString YAMLErrorWithLocations where
	toString {error, locations} =
		concat5
			"Error occurred while constructing YAML: "
			(toString error)
			"."
			(if (isEmpty locations) ("") ("The following hints were provided for solving the error: "))
			(join ". " (map (\l -> "Error occurred " +++ toString l) locations))

instance toString ErrorLocation where
	toString (ADT a) = concat3 "while parsing ADT \"" a "\""
	toString (Constructor c) = concat3 "while parsing constructor \"" c "\""
	toString (Record r) = concat3 "while parsing record \"" r "\""
	toString (Field f) = concat3 "while parsing field \"" f "\""
	toString (SequenceIndex i) = "at sequence index " +++ (toString i)

severityCorrespondingTo :: !'Eastwood.Diagnostic'.DiagnosticSeverity -> 'LSP.Diagnostic'.DiagnosticSeverity
severityCorrespondingTo 'Eastwood.Diagnostic'.Error       = 'LSP.Diagnostic'.Error
severityCorrespondingTo 'Eastwood.Diagnostic'.Warning     = 'LSP.Diagnostic'.Warning
severityCorrespondingTo 'Eastwood.Diagnostic'.Hint        = 'LSP.Diagnostic'.Hint
severityCorrespondingTo 'Eastwood.Diagnostic'.Information = 'LSP.Diagnostic'.Information