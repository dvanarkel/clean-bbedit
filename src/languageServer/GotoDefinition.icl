implementation module GotoDefinition

import StdEnv
import StdOverloadedList

import Data.Error
import Data.Func
import Data.Functor
import qualified Data.Foldable
import Data.List
import Data.Maybe
import Data.Tuple
import Text
from Text.Unicode.UChar import isSymbol, isAlphaNum, isPunctuation, instance fromChar UChar, instance toInt UChar,
	instance == UChar
import Text.URI
import System.FilePath
import System.Process

import LSP.BasicTypes
import LSP.RequestMessage
import LSP.ResponseMessage
import LSP.TextDocumentIdentifier
import LSP.Internal.Serialize

import GotoUtil
import Util

onGotoDefinition :: !RequestMessage !EastwoodState !*World -> (!ResponseMessage, !*World)
onGotoDefinition req=:{RequestMessage|id, params = ?Just json} st world
	# {GotoDeclarationOrDefinitionParams|textDocument={TextDocumentIdentifier|uri}} = deserialize json
	# requestPath = uri.uriPath
	# requestMadeFromIcl = takeExtension requestPath == "icl"
	# requestFileBaseName = (dropExtension $ takeFileName requestPath)
	# (mbPrerequisites, world) = gotoPrerequisitesFor req st world
	| isError mbPrerequisites = (fromError mbPrerequisites, world)
	# {line, charNr, rootPath, cleanHomeLibs} = fromOk mbPrerequisites
	# mbSearchTerms = grepSearchTermFor line charNr
	| isError mbSearchTerms = (fromError mbSearchTerms, world)
	// Using the search terms, grep is executed to find the file names and line numbers of the definitions that match.
	# {	  generalSearchTerm , ctorPrecededByPipeOrEqualsOnPrecedingLineSearchTerm
		, requestFileSearchTerm, requestFileFuncDefSearchTerm, requestFileFuncDefWithoutTypeAnnotationSearchTerm
		, requestFileFuncDefWithoutTypeAnnotationSearchTermSpecialCase
		} = fromOk mbSearchTerms
	# (mbResGeneralCase, world) =
		grepResultsForSearchTerm
			Definition (?Just $ takeFileName requestPath) generalSearchTerm rootPath cleanHomeLibs []
			singleLineGrepStdoutToFilePathAndLineNr id world
	| isError mbResGeneralCase = (fromError mbResGeneralCase, world)
	# (mbResCtorPipeOrEqualsSameLine, world)
		= 'Data.Foldable'.foldl`
			(\(_, world) searchTerm ->
				grepResultsForSearchTerm Definition ?None searchTerm rootPath cleanHomeLibs ["-B", "3"]
					(surroundingLineGrepStdoutToFilePathAndLineNr
						(flip IsMember whitespaceChars)
						[!'=', '|']
						True
					)
					id world
			)
			(Ok [], world)
			ctorPrecededByPipeOrEqualsOnPrecedingLineSearchTerm
	| isError mbResCtorPipeOrEqualsSameLine = (fromError mbResCtorPipeOrEqualsSameLine, world)
	# (mbResLocalSearch, world) =
		if requestMadeFromIcl
			(grepResultsForSearchTerm
				(SingleFile $ takeFileName requestPath) ?None requestFileSearchTerm requestPath [] []
				 singleLineGrepStdoutToFilePathAndLineNr id world
			)
			(Ok [], world)
	| isError mbResLocalSearch = (fromError mbResLocalSearch, world)
	// Tries to find a type annotated func definition in the .icl
	# (mbResLocalTypeAnnotatedFunc, world) =
		if requestMadeFromIcl
			(grepResultsForSearchTerm
				(SingleFile $ takeFileName requestPath) ?None requestFileFuncDefSearchTerm requestPath [] []
				singleLineGrepStdoutToFilePathAndLineNr id world
			)
			(Ok [], world)
	| isError mbResLocalSearch = (fromError mbResLocalSearch, world)
	// Finds a local non type annotated function where the function args are included in the succeeding line.
	# (mbResLocalNonTypeAnnotatedFuncArgsNextLine, world) =
		// If the request was not made from a .icl or we found a type annotated function in the .icl we do not search.
		if  (	requestMadeFromIcl
				&& (mbResLocalTypeAnnotatedFunc=:(Ok []) && (mbResCtorPipeOrEqualsSameLine=:(Ok [])))
		 	)
			(grepResultsForSearchTerm
					(SingleFile $ takeFileName requestPath)
					?None
					requestFileFuncDefWithoutTypeAnnotationSearchTermSpecialCase
					requestPath
					[]
					["-A", "3"]
					(surroundingLineGrepStdoutToFilePathAndLineNr
						(flip IsMember alphabeticAndWhitespaceChars)
						[!'=', '|', '#', ':']
						False // Read from front to end of line.
					)
					id world
			)
			(Ok [], world)
	| isError mbResLocalNonTypeAnnotatedFuncArgsNextLine = (fromError mbResLocalNonTypeAnnotatedFuncArgsNextLine, world)
	// Tries to find a type annotated func definition in the .icl
	# (mbResLocalNonTypeAnnotatedFunc, world) =
		if	(	requestMadeFromIcl &&
				mbResLocalTypeAnnotatedFunc=:(Ok []) &&
				mbResLocalNonTypeAnnotatedFuncArgsNextLine=:(Ok [])
			)
			(grepResultsForSearchTerm
				(SingleFile $ requestFileBaseName +++ ".icl") ?None
				requestFileFuncDefWithoutTypeAnnotationSearchTerm requestPath [] []
				singleLineGrepStdoutToFilePathAndLineNr id world
			)
			(Ok [], world)
	| isError mbResLocalNonTypeAnnotatedFunc = (fromError mbResLocalNonTypeAnnotatedFunc, world)
	// Only return single result.
	# mbResLocalNonTypeAnnotatedFunc =
		case mbResLocalNonTypeAnnotatedFunc of
			Ok [x:xs] = Ok [x]
			_ = Ok []
	# results =
		fromOk mbResGeneralCase ++
		fromOk mbResCtorPipeOrEqualsSameLine ++
		fromOk mbResLocalSearch ++
		fromOk mbResLocalTypeAnnotatedFunc ++
		fromOk mbResLocalNonTypeAnnotatedFunc ++
		fromOk mbResLocalNonTypeAnnotatedFuncArgsNextLine
	// For every tuple of fileName and lineNumber, a Location is generated to be sent back to the client.
	# locations = [! l \\ l <- catMaybes $ fileAndLineToLocation <$> results !]
	= (locationResponse id locations, world)
where
	/**
	 * This function retrieves the search term that is passed to grep which is used for finding the definition.
	 * If the function succeeds, a record containing search terms is returned.
	 *
	 * @param The line number for which a declaration was requested.
	 * @param The character number that was selected when a declaration request was made.
	 * @result an error response to be sent back to the client or the search terms used by grep.
	 */
	grepSearchTermFor :: !String !UInt -> MaybeError ResponseMessage GotoDefinitionGrepSearchTerms
	grepSearchTermFor line uIntChar=:(UInt charNr)
		| size line - 1 < charNr || charNr < 0 =
			Error $
				errorResponse id ParseError
					"Go to definition failed, file was not saved or empty line was selected."
		# firstUnicodeChar = fromChar $ select line charNr
		// If the first char is a space, comma, \n, or \t, go backwards
		// When a declaration is requested when a whole term is selected the character ends up being the first char
		// after the term, the same holds when attempting to go to the declaration when selecting a lookBackCharacter.
		| IsMember firstUnicodeChar lookBackCharacters = grepSearchTermFor line (UInt (charNr - 1))
		// This case is added to deal with going to the declaration of an infix function that is used prefix
		// and selecting the ( character.
		| firstUnicodeChar == lookForwardCharacter = grepSearchTermFor line (UInt (charNr + 1))
		// It should not be attempted to go the declaration of special syntax symbols.
		| isSpecialCharacter firstUnicodeChar =
			Error $
				errorResponse id ParseError "it is not possible to go to the definition of a special syntax symbol."
		// This is the general case.
		| isSymbol firstUnicodeChar || isAlphaNum firstUnicodeChar || isPunctuation firstUnicodeChar
			# searchTerm =
				removeUnwantedSymbolsFromSearchTerm $ retrieveSearchTerm (stopPredicate firstUnicodeChar) line uIntChar
			# searchTerm =
				if (isInfixOf [c \\ c <-:"{|"] [c \\ c <-: searchTerm] || isInfix searchTerm)
					searchTerm
					// If the search term does not contain a generic kind specification, we parse again using a
					// more strict predicate to avoid a problem with [(a,b):f].
					// If this is not done the search term for [(a,b):f] would become abf instead of f.
					(removeUnwantedSymbolsFromSearchTerm
						$ retrieveSearchTerm stopPredicateAfterGenericKindSpecificationWasNotFound line uIntChar
					)
			# searchTerm = escapeRegexCharactersInSearchTerm searchTerm
			// Grep terms which should be applied to both local .icl search term and any other icl.
			# commonGrepTerms =
				concat
					// Only search for types when the term starts with an uppercase character.
					[ if (grepTypeSearchTerm searchTerm == "") "" (grepTypeSearchTerm searchTerm +++ "|")
					, grepGenericSearchTerm searchTerm
					, "|"
					, grepClassSearchTerm searchTerm
					, "|"
					, grepMacroSearchTerm searchTerm
					, "|"
					, grepTypeSynonymSearchTerm searchTerm
					, "|"
					, grepNewOrAbstractTypeSearchTerm searchTerm
					// Only search for constructors if the term starts with an uppercase character.
					, if (grepConstructorSearchTerm searchTerm == "") "" ("|" +++ grepConstructorSearchTerm searchTerm)
					]
			= Ok $
				{ generalSearchTerm = concat3 commonGrepTerms "|" (grepFuncSearchTerm searchTerm)
				, ctorPrecededByPipeOrEqualsOnPrecedingLineSearchTerm =
					grepConstructorSearchTermSpecialCase searchTerm
				, requestFileSearchTerm =
					commonGrepTerms
				, requestFileFuncDefSearchTerm = grepFuncSearchTerm searchTerm
				, requestFileFuncDefWithoutTypeAnnotationSearchTerm = grepNoTypeAnnotationFuncSearchTerm searchTerm
				, requestFileFuncDefWithoutTypeAnnotationSearchTermSpecialCase
					= grepNoTypeAnnotationFuncSearchTermSpecialCase searchTerm
				}
		= Error $
			errorResponse
			id
			ParseError
			("Unrecognised char with unicode : " +++ (toString $ toInt firstUnicodeChar))

//* The grep func definition search pattern is adjusted based on
//* whether an infix function or a prefix function was parsed.
grepNoTypeAnnotationFuncSearchTerm :: !String -> String
grepNoTypeAnnotationFuncSearchTerm searchTerm
	=
		if (isInfix searchTerm)
			(	let
					// Characters which should be escaped to avoid them being seen as regex..
					// See https://riptutorial.com/regex/example/15848/what-characters-need-to-be-escaped.
					charactersToEscape
						= [!'[', ']', '(', ')', '{', '}', '*', '+', '?', '|', '^', '$', '.', '\\']
					// Every character that should be escaped results in 2 characters (one for the \)
					escapedSearchTerm
						= concat $
							[ if (IsMember c charactersToEscape) ("\\" +++ toString c) (toString c)
								\\ c <-: searchTerm
							]
				// infix[lr]? indicates infix followed by l, r, or nothing.
				in concat5 "\\(?" escapedSearchTerm "\\)?" anyAmountOfCharacters "="
			)
			(concat
				[ lineStartsWith
				, anyAmountOfWhitespace
				, searchTerm
				, atleastOneWhiteSpace
				, anyAmountOfCharacters
				// Not preceded by :, := or = (so macros are not found)
				, "(?<!:|:=|=)"
				, "="
				]
			)

//* The grep func definition search pattern is adjusted based on
//* whether an infix function or a prefix function was parsed.
grepNoTypeAnnotationFuncSearchTermSpecialCase :: !String -> String
grepNoTypeAnnotationFuncSearchTermSpecialCase searchTerm
	=
		if (isInfix searchTerm)
			(	let
					// Characters which should be escaped to avoid them being seen as regex..
					// See https://riptutorial.com/regex/example/15848/what-characters-need-to-be-escaped.
					charactersToEscape
						= [!'[', ']', '(', ')', '{', '}', '*', '+', '?', '|', '^', '$', '.', '\\']
					// Every character that should be escaped results in 2 characters (one for the \)
					escapedSearchTerm
						= concat $
							[ if (IsMember c charactersToEscape) ("\\" +++ toString c) (toString c)
								\\ c <-: searchTerm
							]
				// infix[lr]? indicates infix followed by l, r, or nothing.
				in concat4 "\\(?" escapedSearchTerm "\\)?" anyAmountOfCharacters
			)
			(concat
				[ lineStartsWith
				, anyAmountOfWhitespace
				, searchTerm
				]
			)

:: GotoDefinitionGrepSearchTerms =
	{ generalSearchTerm :: !String
		//* contains search term applied to all relevant .dcl and .icl files
		//* Except for the file from which the request was made if it is a .icl.
	, ctorPrecededByPipeOrEqualsOnPrecedingLineSearchTerm :: !?String
		//* If the search type could be a constructor, this search term searches for Constructors that are
		//* Preceded by | or = on the preceding line.
	, requestFileSearchTerm :: !String
		//* The search term applied to the .icl from which the request was made if applicable.
		//* Used to find local definitions.
	, requestFileFuncDefSearchTerm :: !String
		//* The search term applied to the .icl from which the request was made if applicable.
		//* Used to find function definitions within the local file. Separate search term to only look for
		//* For func definitions without a type annotation if no function with a type definition could be found.
	, requestFileFuncDefWithoutTypeAnnotationSearchTerm :: !String
		//* The search term applied to the .icl from which the request was made if applicable.
		//* Used to find func definitions within the local file.
		//* Only used if no function with a type definition could be found.
	, requestFileFuncDefWithoutTypeAnnotationSearchTermSpecialCase :: !String
	}
