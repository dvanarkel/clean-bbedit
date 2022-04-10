implementation module GotoUtil

import StdEnv
import StdOverloadedList

import Data.Array
import Data.Bool
import Data.Error
import Data.Func
import Data.Functor
import Data.GenEq
import Data.List
import Data.Maybe
import Data.Tuple
import System.Environment
import System.File
import System.FilePath
import System.Process
import Text
import Text.Encodings.UrlEncoding
from Text.Unicode.UChar import :: UChar, isAlphaNum, isSymbol, isPunctuation, instance fromChar UChar, instance == UChar
	, instance toChar UChar
import Text.URI

import LSP.BasicTypes
import LSP.Internal.Serialize
import LSP.Location
import LSP.TextDocumentIdentifier
import qualified LSP.Position
from LSP.Position import :: Position {..}
from LSP.Position import qualified :: Position {..}
import qualified LSP.Range
from LSP.Range import :: Range {..}
import LSP.ResponseMessage
import LSP.RequestMessage
import LSP.TextDocumentIdentifier

import qualified Eastwood.Range
from Eastwood.Range import qualified :: Range {..}, :: CharacterRange, :: Position {..}
import Eastwood.Util.FileFinder

import Config
import Constants
import Util

derive gLSPJSONDecode GotoDeclarationOrDefinitionParams

gotoPrerequisitesFor
	:: !RequestMessage !EastwoodState !*World -> (MaybeError ResponseMessage GotoPrerequisites, !*World)
gotoPrerequisitesFor req=:{RequestMessage|id, params= ?Just json} st=:{EastwoodState|workspaceFolders} world
	# {GotoDeclarationOrDefinitionParams|textDocument={TextDocumentIdentifier|uri}, position} = deserialize json
	# (mbLine, world) = getLineOfDeclarationRequest id uri position world
	| isError mbLine = (liftError mbLine, world)
	# line = fromOk mbLine
	// NB: charNr does not mean column number, it is the number of the character within the line.
	// E.g tab = x cols 1 char.
	# charNr = position.'LSP.Position'.Position.character
	// Parse the grep search term for which a declaration was requested.
	// The root path is necessary because otherwise grep returns relative paths and the client needs absolute paths.
	// The config path is the root path.
	# (mbConfigPath, world) = findSearchPath PROJECT_FILENAME workspaceFolders world
	| isNone mbConfigPath =
		(Error $ errorResponse id InternalError ("Could not find absolute path of " +++ PROJECT_FILENAME)
		, world
		)
	# searchPath = fromJust mbConfigPath
	// We remove symlinks/../. to get the actual full path.
	# (mbRootPath, world) = getFullPathName searchPath world
	| isError mbRootPath =
		( Error $ errorResponse id InternalError ("Could not find absolute path of " +++ PROJECT_FILENAME)
		, world
		)
	# rootPath = fromOk mbRootPath
	// The libraries in CLEAN_HOME_ENV_VAR/LIBS_PATH included in Eastwood.yml are searched by grep so CLEAN_HOME_ENV_VAR
	// is retrieved.
	# (mbCleanHome, world) = getEnvironmentVariable CLEAN_HOME_ENV_VAR world
	| isNone mbCleanHome =
		( Error $ errorResponse id UnknownErrorCode "Could not find CLEAN_HOME environment variable."
		, world
		)
	# cleanHomePath = fromJust mbCleanHome
	// The CLEAN_HOME libraries included in Eastwood.yml are searched by grep so Eastwood.yml is fetched.
	# (mbConfig, world) = fetchConfig workspaceFolders world
	| isError mbConfig
		= (Error $ errorResponse id UnknownErrorCode (fromError mbConfig), world)
	# {libraries} = fromOk mbConfig
	// The libraries in Eastwood.yml are transformed to their full paths.
	# (mbCleanHomeLibs, world)
		= mapSt (\l -> getFullPathName (cleanHomePath </> LIBS_PATH </> l)) libraries world
	| any isError mbCleanHomeLibs =
		( Error $ errorResponse id UnknownErrorCode "Could not get full path of library included in Eastwood.yml."
		, world
		)
	# cleanHomeLibs = map fromOk mbCleanHomeLibs
	= (Ok {line=line, charNr=charNr,rootPath=rootPath, cleanHomeLibs=cleanHomeLibs}, world)

/**
 * This function returns the line for which a go to declaration request was made in string form.
 *
 * @param The URI that is provided by the client, indicates file for which request was made.
 * @param The position within the file for which the request was made
 * @param World
 * @result an error response to send back to the client in case of failure or the line of the request as a string.
 * @result World
 */
getLineOfDeclarationRequest
	:: !RequestId !URI 'LSP.Position'.Position !*World -> (!MaybeError ResponseMessage String, !*World)
getLineOfDeclarationRequest id uri position world
	// Decode URL as clean expects filepaths that are not URL-encoded while LSP supplies URL encoded filepaths
	# (mbLines, world) = readFileLines (urlDecode uri.uriPath) world
	| isError mbLines =
		(Error $
			errorResponse id ContentModified (concat3 "The file located at " uri.uriPath "was not found.")
		, world
		)
	# lines = fromOk mbLines
	# (UInt lineNr) = position.'LSP.Position'.line
	# mbLine = lines !? lineNr
	| isNone mbLine =
		(Error $
			errorResponse
				id
				ParseError
				(concat4 "The file located at " uri.uriPath "does no longer contain line number " (toString lineNr))
		, world
		)
	// The line was found.
	= (Ok $ fromJust mbLine, world)

filterSurroundingLinesForPredUntilStopSymbol :: !(Char -> Bool) ![!Char] !Bool ![String] -> [(Int, String)]
filterSurroundingLinesForPredUntilStopSymbol pred stopSymbols filterInReverse lines
	# indexedLines = [ (idx,line) \\ line <- lines & idx <- [0..] ]
	# (surroundingLinesWIdx, resultLinesWIdx) =
		partition
			(\(_, line)
				# firstColon = indexOf ":" line
				// If the string does not contain a hyphen at all we filter it since it cannot be the previous line.
				// If the string does not contain a column then it is a surrounding line.
				// If last line number seperator is not included before the first colon we filter it.
				// This filter has problems when - or : are added as comments in the same line as the previous line
				// of the constructor, this can be the case due to comments.
				-> if (lastLineNumberSeparator line == -1)
					False
					(if (indexOf ":" line == -1) True (lastLineNumberSeparator line <= firstColon - 1))
			)
			indexedLines
	| resultLinesWIdx == [] = []
	# isLookAhead = (fst $ hd resultLinesWIdx) == 0
	= resultsForFilter resultLinesWIdx (map snd surroundingLinesWIdx) isLookAhead
where
	lastLineNumberSeparator :: !String -> Int
	lastLineNumberSeparator line = indexOfLastHyphenLineNumberSeperator line

	// Assumption made: the grep result does not contain -linenumber- after the actual line number.
	// The format is file-lineNumber-match. so if match contains -onlynumbers- this fails.
	// This should be impossible to have unless comments are used within the same line.
	indexOfLastHyphenLineNumberSeperator :: !String -> Int
	indexOfLastHyphenLineNumberSeperator line
		// We parse in reverse because it makes it easier because file names can contain hyphens themselves.
		= indexOfLastHyphenLineNumberSeperator` $ reverseArr line
	where
		indexOfLastHyphenLineNumberSeperator` reversedLine
			// Get the first hyphen in the reversed line.
			# indexFirstHyphen = indexOf "-" reversedLine
			// There is no hyphen so no line number seperator either, this uses the axiom that the previous line.
			// always includes a line number indication of the form -lineNumber-
			| indexFirstHyphen == -1 = -1
			# hyphenisPartOfArrow = if (indexFirstHyphen > 0) (select reversedLine (indexFirstHyphen - 1) == '>') False
			| hyphenisPartOfArrow = indexOfLastHyphenLineNumberSeperator` $ dropChars (indexFirstHyphen + 1) reversedLine
			// We read from the first hyphen and discard it to find the next hyphen.
			# lineFromFirstHyphen = dropChars (indexFirstHyphen + 1) reversedLine
			# indexFollowingHyphen = indexOf "-" lineFromFirstHyphen
			| indexFollowingHyphen == -1 = -1
			# charactersBetweenHyphens = takeArr indexFollowingHyphen lineFromFirstHyphen
			#! charactersBetweenHyphens =  charactersBetweenHyphens
			// Look if all characters between the hyphens are numbers.
			# allCharactersBetweenHyphensAreNumbers
				= All isDigit [c \\ c <-: charactersBetweenHyphens]
			// Found the linenumber seperator hyphen (assumption, the line does not include -number- before the
			// line number seperator).
			| allCharactersBetweenHyphensAreNumbers
				= lastIndexOf "-" (reverseArr reversedLine)
			= -1

	resultsForFilter :: ![(Int, String)] ![String] !Bool -> [(Int, String)]
	resultsForFilter resultLines surroundingLines isLookAhead
		=
		case resultLines of
			[(_,_)] = checkFilter surroundingLines
			[(firstIdx, _):sndRes=:(sndIdx,_):rest] =
				checkFilter (take (sndIdx - firstIdx) surroundingLines) ++
				resultsForFilter [sndRes:rest] (drop (sndIdx - firstIdx) surroundingLines) isLookAhead
	where
		checkFilter :: ![String] -> [(Int, String)]
		checkFilter surroundingLinesToCheck
			# surroundingLinesToCheckOrdered =  if isLookAhead surroundingLinesToCheck (reverse surroundingLinesToCheck)
			# surroundingLinesWRelPos =
				[ (if isLookAhead (~pos) pos,l) \\ l <- surroundingLinesToCheckOrdered & pos <- [1..]]
			= checkFilter` surroundingLinesWRelPos
		where
			checkFilter` :: ![(Int, String)] -> [(Int, String)]
			checkFilter` [] = []
			checkFilter` [(pos,l):ls]
				= maybe (checkFilter` ls) (\holds -> if holds [(pos,l)] [])
					$ predHoldsUntilStopSymbolIsFound (dropChars (lastLineNumberSeparator l + 1) l)

	predHoldsUntilStopSymbolIsFound :: !String -> ?Bool
	predHoldsUntilStopSymbolIsFound line
		= predHoldsUntilStopSymbolIsFound` pred stopSymbols $ if filterInReverse reverse id $ [c \\ c <-:line]
	where
		predHoldsUntilStopSymbolIsFound` :: !(Char -> Bool) [!Char] ![Char] -> ?Bool
		predHoldsUntilStopSymbolIsFound` pred stopSymbols [c:cs]
			| IsMember c stopSymbols
				// Found a stopSymbol at the start of the line, incidating that search became out of scope.
				| filterInReverse == False && [c:cs] == (fromString line) = ?Just False
				= ?Just True
			| pred c = predHoldsUntilStopSymbolIsFound` pred stopSymbols cs
			= ?Just False
		predHoldsUntilStopSymbolIsFound` _ _ [] = ?None

grepResultsForSearchTerm ::
	!SearchType !(?FilePath) !String !FilePath ![FilePath] ![String] !(String -> [(String, Int)]) !RequestId !*World
	-> (!MaybeError ResponseMessage [(String, Int)], !*World)
grepResultsForSearchTerm searchType mbFileToExclude searchTerm rootPath cleanHomeLibs additionalArgs
	grepOutputToResults id world
	# (mbGrepResult, world) =
		executeGrep mbFileToExclude (searchTypeToSearchPattern searchType) searchTerm rootPath
			cleanHomeLibs additionalArgs world
	| isError mbGrepResult
		= (Error $ errorResponse id InternalError "grep failed when searching for single line results.", world)
	# {stdout} = fromOk mbGrepResult
	# results = grepOutputToResults stdout
	= (Ok results, world)

executeGrep ::
	!(?FilePath) !String !String !FilePath ![FilePath] ![String] !*World -> (!MaybeOSError ProcessResult, !*World)
executeGrep mbFileToExclude searchPattern searchTerm rootPath cleanHomeLibs additionalArgs world =
	// Call grep using the regular search term to find the matching declarations.
	// -P enables perl regexp, -r recurses through all files, -n gives line number,
	// -H prints the filename for each match.
	// -w matches whole words only (e.g: :: Maybe matches :: Maybe but not :: MaybeOSError).
	// --include \*.dcl or \*.icl makes sure only dcl/icls files are examined by grep, respectively.
	// --exclude excludes a file from the search.
	// rootPath and cleanHomeLibs are searched for matches.
	callProcessWithOutput
		"grep"
		(	additionalArgs ++
			["-P", searchTerm
			, "-r"
			, "-n"
			, "-H"
			, "-w"
			, "--include", searchPattern
			, rootPath
			]
			++ maybe [] (\fileToExclude -> ["--exclude", fileToExclude]) mbFileToExclude
			++ cleanHomeLibs
		)
		?None
		world

// Example grep output (stdout):
// src/languageServer/GotoUtil.dcl:59:resultsForCtorWithPipeOrEqualsOnThePrecedingLine ::
// <empty line at end>
singleLineGrepStdoutToFilePathAndLineNr :: !String -> [(FilePath, Int)]
singleLineGrepStdoutToFilePathAndLineNr stdout =
	(	(\[fileName, lineNr] -> ((fileName, toInt lineNr))) o take 2 o split ":" <$>
			(init $ split "\n" stdout)
	)

// Example grep output:
// src/languageServer/GotoUtil.dcl-58-
// src/languageServer/GotoUtil.dcl:59:resultsForCtorWithPipeOrEqualsOnThePrecedingLine ::
// <empty line at end>
//
// - is used as a seperator for the results, as grep uses `-` as a group seperator.
// when previous lines are shown instead of `:`.
// In this case, the previous line will be found so +1 is added to the line number to return the line
// where the actual constructor is located.
// The filename can contain - itself so this is accounted for.
// The actual match is at the end so this is dropped, this is followed by the lineNr and strs that when
// concatenated form the filename, this does not account for the match containing -.
// So if the constructor definition itself contains hyphens in comments this breaks.
surroundingLineGrepStdoutToFilePathAndLineNr :: !(Char -> Bool) [!Char] !Bool !String -> [(FilePath, Int)]
surroundingLineGrepStdoutToFilePathAndLineNr holdsBeforeStop stopSymbols reverseStdoutLine stdout =
	(	(\(posRelativeToResult, [lineNr:fileName]) -> (join "-" $ reverse $ fileName, toInt lineNr + posRelativeToResult))
		o appSnd (drop 1 o reverse o split "-")
		<$>
		(filterSurroundingLinesForPredUntilStopSymbol
			holdsBeforeStop
			stopSymbols
			reverseStdoutLine
			(init $ split "\n" stdout)
		)
	)

searchTypeToSearchPattern :: !SearchType -> String
searchTypeToSearchPattern Declaration = "\*.dcl"
searchTypeToSearchPattern Definition = "\*.icl"
searchTypeToSearchPattern (SingleFile filePath) = filePath

whitespaceChars :: [!Char]
whitespaceChars = [!' ', '\t', '\r', '\n', '\v', '\f']

alphabeticAndWhitespaceChars :: [!Char]
alphabeticAndWhitespaceChars = [!'a'..'z'] ++| whitespaceChars

fileAndLineToLocation :: !(!String, !Int) -> ?Location
fileAndLineToLocation (filePath, lineNr)
	# fileUri =
		parseURI $ "file://" </>
			replaceFileName
				filePath
				// The filename is URL encoded as this is expected by LSP.
				(concat3
					(urlEncode $ dropExtension $ takeFileName filePath)
					extSeparatorString
					(takeExtension filePath)
				)
	| isNone fileUri = ?None
	= ?Just $
		{ Location
		| uri = fromJust fileUri
		, range = rangeCorrespondingTo
			{ 'Eastwood.Range'.Range
			| 'Eastwood.Range'.start={'Eastwood.Range'.Position|'Eastwood.Range'.line=lineNr-1, 'Eastwood.Range'.character=0}
			, 'Eastwood.Range'.end={'Eastwood.Range'.Position|'Eastwood.Range'.line=lineNr-1, 'Eastwood.Range'.character=0}
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

lookBackCharacters :: [!UChar]
lookBackCharacters =: Map fromChar [' ', ',', '\n', '\t', ')']

lookForwardCharacter :: UChar
lookForwardCharacter =: fromChar '('

isSpecialCharacter :: !UChar -> Bool
isSpecialCharacter uc = IsMember uc specialCharacters
where
	specialCharacters :: [!UChar]
	specialCharacters = Map fromChar ['[', ']', ';', '\"', '\'', ',']

genericKindSpecificationSymbols :: [UChar]
genericKindSpecificationSymbols = (map fromChar ['{', '|', '*', '|', '}', '(', ')'])

stopPredicate :: !UChar -> (UChar -> Bool)
stopPredicate uc = if (isAlphaNum uc) stopPredicatePrefix stopPredicateInfixOrGenericKindSpec
where
	stopPredicateInfixOrGenericKindSpec :: !UChar -> Bool
	stopPredicateInfixOrGenericKindSpec uc =
		not (isAlphaNum uc) && not (isSymbol uc || isPunctuation uc) || isSpecialCharacter uc

stopPredicatePrefix :: !UChar -> Bool
stopPredicatePrefix uc = not (isAlphaNum uc) && not (isMember uc [fromChar '_':genericKindSpecificationSymbols])

removeUnwantedSymbolsFromSearchTerm :: !String -> String
removeUnwantedSymbolsFromSearchTerm searchTerm =
	{c \\ c <-: searchTerm |
		// _ is kept because of macros.
		(isNotInfix searchTerm --> not (isSymbol (fromChar c) || isPunctuation (fromChar c)) || c == '_') &&
		(isInfix searchTerm --> (not $ IsMember (fromChar c) [!'(', ')']))
	}

isNotInfix :: !String -> Bool
isNotInfix searchTerm = any isAlphaNum [fromChar c \\ c <-: searchTerm]

isInfix :: !String -> Bool
isInfix searchTerm = not $ isNotInfix searchTerm

retrieveSearchTerm :: !(UChar -> Bool) !String !UInt -> String
retrieveSearchTerm predicate line (UInt charNr) =
	toString $
		(Reverse $ parseSearchTerm line predicate (Reverse [!0..charNr-1!])) ++|
		parseSearchTerm line predicate [!charNr..size line - 1!]

/**
 * This function parses the raw search term for which a declaration/definition was requested without adding regexp
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

stopPredicateAfterGenericKindSpecificationWasNotFound :: !UChar -> Bool
stopPredicateAfterGenericKindSpecificationWasNotFound uc
	= stopPredicatePrefix uc || isMember uc (map fromChar ['{', '|', '*', '|', '}', '(', ')'])

//* Types always start with an uppercase character.
grepTypeSearchTerm :: !String -> String
grepTypeSearchTerm searchTerm =
	if (startsWithUpper searchTerm)
		(concat5 lineStartsWith "::" anyAmountOfWhitespace maybeAsterisk searchTerm)
		""

// The ^ indicates that the term that follows should not be preceded by any characters.
// This is used to avoid finding imports as declarations terms are never preceded by characters.
lineStartsWith :: String
lineStartsWith = "^"

startsWithUpper :: !String -> Bool
startsWithUpper s = isUpper $ select s 0

// Further processing has to be done for constructors that have the | or = on the previous line.
// In this case, the constructor has to be preceded by at least one whitespace only.
// For this we return a seperate search term since we have to process the previous line.
grepConstructorSearchTermSpecialCase :: !String -> ?String
grepConstructorSearchTermSpecialCase searchTerm =
	if (startsWithUpper searchTerm) (?Just $ concat3 lineStartsWith atleastOneWhiteSpace searchTerm) ?None

//* The grep func definition search pattern is adjusted based on
//* whether an infix function or a prefix function was parsed.
grepFuncSearchTerm :: !String -> String
grepFuncSearchTerm searchTerm =
	if (isInfix searchTerm)
		// infix[lr]? indicates infix followed by l, r, or nothing.
		(concat5  "\\(" searchTerm "\\)" atleastOneWhiteSpace "infix[lr]?")
		(concat3 searchTerm anyAmountOfWhitespace "::" )

grepGenericSearchTerm :: !String -> String
grepGenericSearchTerm searchTerm = concat4 lineStartsWith "generic" atleastOneWhiteSpace searchTerm

grepClassSearchTerm :: !String -> String
grepClassSearchTerm searchTerm = concat4 lineStartsWith "class" atleastOneWhiteSpace searchTerm

grepMacroSearchTerm :: !String -> String
grepMacroSearchTerm searchTerm =
	concat
		[ lineStartsWith
		, searchTerm
		, "("
		, atleastOneWhiteSpace
		, anyAmountOfCharacters
		, ":=="
		, "|"
		, ":=="
		, ")"
		]

grepNewOrAbstractTypeSearchTerm :: !String -> String
grepNewOrAbstractTypeSearchTerm searchTerm
	= concat
		[ lineStartsWith
		, "::"
		, anyAmountOfWhitespace
		, maybeBang
		, maybeUniqOrCoercible
		, anyAmountOfWhitespace
		, searchTerm
		, "("
		, atleastOneWhiteSpace
		, anyAmountOfCharacters
		, "=:"
		, "|"
		, "=:"
		, ")"
		]

grepTypeSynonymSearchTerm :: !String -> String
grepTypeSynonymSearchTerm searchTerm
	= concat
		[ lineStartsWith
		, "::"
		, anyAmountOfWhitespace
		, maybeBang
		, maybeUniqOrCoercible
		, anyAmountOfWhitespace
		, searchTerm
		, "("
		, atleastOneWhiteSpace
		, anyAmountOfCharacters
		, ":=="
		, "|"
		, ":=="
		, ")"
		]

grepConstructorSearchTerm :: !String -> String
grepConstructorSearchTerm searchTerm
	// Constructors always start with a uppercase letter, so do not search if this is not the case.
	// The pipe or = preceding the constructor may be preceded by either
	// 1: :: followed by any combination of characters followed by | or = followed by
	// at least one whitespace followed by the search term
	// 2: At least one white space followed by any combination of characters followed by | or = followed by
	// at least one whitespace followed by the search term.
	= if (startsWithUpper searchTerm)
		(concat
			[ lineStartsWith
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

atleastOneWhiteSpace :: String
atleastOneWhiteSpace = "(\\s+)"

anyAmountOfWhitespace :: String
anyAmountOfWhitespace = "(\\s*)"

anyAmountOfCharacters :: String
anyAmountOfCharacters = ".*"

//* Optional ! character.
maybeBang :: String
maybeBang = "(!?)"

maybeAsterisk :: String
maybeAsterisk = "(\\*?)"

//* Optional * or . character.
maybeUniqOrCoercible :: String
maybeUniqOrCoercible = "(\\*|\\.)?"

atleastOneCharacter :: String
atleastOneCharacter = ".+"

pipeOrEquals :: String
pipeOrEquals ="\\||="

escapeRegexCharactersInSearchTerm :: !String -> String
escapeRegexCharactersInSearchTerm searchTerm
	# charactersToEscape = [!'[', ']', '(', ')', '{', '}', '*', '+', '?', '|', '^', '$', '.', '\\']
	= concat $
		[ if (IsMember c charactersToEscape) ("\\" +++ toString c) (toString c)
			\\ c <-: searchTerm
		]

instance == SearchType derive gEq
