implementation module GotoDefinition

import StdEnv
import StdOverloadedList

import Data.Error
import Data.Func
import Data.Functor
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

onGotoDefinition :: !RequestMessage !EastwoodState !*World -> (!ResponseMessage, !EastwoodState, !*World)
onGotoDefinition req=:{RequestMessage|id, params = ?Just json} st world
	# {GotoDeclarationOrDefinitionParams|textDocument={TextDocumentIdentifier|uri}} = deserialize json
	# requestPath = uri.uriPath
	# requestMadeFromIcl = takeExtension requestPath == "icl"
	# requestFileBaseName = (dropExtension $ takeFileName requestPath)
	# (mbPrerequisites, st, world) = gotoPrerequisitesFor req st world
	| isError mbPrerequisites = (fromError mbPrerequisites, st, world)
	# {line, charNr, rootPath, cleanHomeLibs} = fromOk mbPrerequisites
	# mbSearchTerms = grepSearchTermFor line charNr
	| isError mbSearchTerms = (fromError mbSearchTerms, st, world)
	// Using the searchString, grep is executed to find the file names and line numbers of the definitions that match.
	// There is a special case for constructors which have the preceding | or = on the preceding line.
	// This requires checking the previous line, hence there is a seperate search term for efficiency reasons.
	// There is also a special search term for finding where functions in the .icl file from which the request is
	// made, if applicable.
	// There is a special search term for finding record fields in .icl files that are preceded by
	// { or , on the previous line.
	# {	generalSearchTerm , ctorPrecededByPipeOrEqualsOnPrecedingLineSearchTerm
		, requestFileSearchTerm, requestFileFuncDefSearchTerm, requestFileFuncDefWithoutTypeAnnotationSearchTerm
		, requestFileFuncDefWithoutTypeAnnotationSearchTermSpecialCase}
		= fromOk mbSearchTerms
	// Call grep using the regular (non special constructor case) search term to find the matching declarations.
	// -P enables perl regexp, -r recurses through all files, -n gives line number, -H includes file name.
	// -w matches whole words only (e.g: :: Maybe matches :: Maybe but not :: MaybeOSError).
	// --include \*.icl makes sure only icl files are examined by grep.
	# (mbGrepResultRegSearchTerm, world)
		= callProcessWithOutput
				"grep"
				(
					[ "-P", generalSearchTerm
					, "-r"
					, "-n"
					, "-H"
					, "-w"
					, "--include", "*.icl"
					, rootPath
					]
					// Exclude the .icl file from which request is being made if applicable.
					++ if requestMadeFromIcl ["--exclude", requestFileBaseName +++ ".icl"] []
					++ cleanHomeLibs
				)
				?None
				world
	| isError mbGrepResultRegSearchTerm
		= (errorResponse id InternalError "grep failed when searching for type definitions.", st, world)
	# {stdout} = fromOk mbGrepResultRegSearchTerm
	# stdoutRegSearchTerm = stdout
	// The stdout for the special constructor case is processed using grep again to find the locations of the
	// Constructors which are preceded by | and = on the preceding line.
	# (mbGrepResultSpecialConstructorCase, world) = case ctorPrecededByPipeOrEqualsOnPrecedingLineSearchTerm of
		?None = (?None, world)
		?Just searchTerm =
			appFst ?Just $
				callProcessWithOutput
					"grep"
						(
							[ "-P", searchTerm
							// -B 1 also selects the previous line.
							, "-B", "1"
							, "-r"
							, "-n"
							, "-w"
							, "--include", "*.icl"
							, rootPath
							] ++ cleanHomeLibs
						)
					?None
					world
	| isJust mbGrepResultSpecialConstructorCase && (isError $ fromJust mbGrepResultSpecialConstructorCase)
		= (errorResponse id InternalError "grep failed when searching for special constructor case.", st, world)
	# stdoutSpecialConstructorCase = case mbGrepResultSpecialConstructorCase of
		// No need to search for a constructor since the searchTerm can not be a constructor.
		?None = ""
		// There is a need to search for a constructor since the searchTerm could be a constructor.
		?Just grepResult
			= (fromOk grepResult).ProcessResult.stdout
	// The stdout for the file from which the request was made.
	# (mbStdoutLocalSearchTerm, world) =
		case requestMadeFromIcl of
			False = (Ok ?None, world)
			True
				# (mbGrepResultLocalSearchTerm, world)
					= callProcessWithOutput
						"grep"
						(
							[ "-P", requestFileSearchTerm
							, "-r"
							, "-n"
							, "-H"
							, "-w"
							// Only apply this search term to the .icl file from which the request is made,
							// if applicable.
							, "--include", requestFileBaseName +++ ".icl"
							, requestPath
							]
						)
						?None
						world
				| isError mbGrepResultLocalSearchTerm
					=
						(Error
							$ errorResponse id InternalError "grep failed when searching for type definitions."
					 	, world
						)
				# {stdout} = fromOk mbGrepResultLocalSearchTerm
				= (Ok $ ?Just stdout, world)
	| isError mbStdoutLocalSearchTerm = (fromError mbStdoutLocalSearchTerm, st, world)
	// The stdout for the file from which the request was made
	// Tries to find a type annotated func definition in the .icl
	# (mbStdoutLocalFuncSearchTerm, world) =
		case requestMadeFromIcl of
			False = (Ok ?None, world)
			True
				# (mbGrepResultLocalFuncSearchTerm, world)
					= callProcessWithOutput
						"grep"
						(
							[ "-P", requestFileFuncDefSearchTerm
							, "-r"
							, "-n"
							, "-H"
							, "-w"
							// Only apply this search term to the .icl file from which the request is made,
							// if applicable.
							, "--include", requestFileBaseName +++ ".icl"
							, requestPath
							]
						)
						?None
						world
				| isError mbGrepResultLocalFuncSearchTerm
					=
						(Error
							$ errorResponse id InternalError "grep failed when searching for type definitions."
					 	, world
						)
				# {stdout} = fromOk mbGrepResultLocalFuncSearchTerm
				= (Ok $ ?Just stdout, world)
	| isError mbStdoutLocalFuncSearchTerm = (fromError mbStdoutLocalFuncSearchTerm, st, world)
	# (mbStdoutLocalNonTypeAnnotedFuncSearchTerm, world) =
		// If the request was not made from a .icl or we found a type annotated function in the .icl we do not search.
		case requestMadeFromIcl && mbStdoutLocalFuncSearchTerm =: (Ok (?Just "")) of
			False = (Ok ?None, world)
			True
				# (mbGrepResultLocalFuncSearchTerm, world)
					= callProcessWithOutput
						"grep"
						(
							[ "-P", requestFileFuncDefWithoutTypeAnnotationSearchTerm
							, "-r"
							, "-n"
							, "-H"
							, "-w"
							// Only apply this search term to the .icl file from which the request is made,
							// if applicable.
							, "--include", requestFileBaseName +++ ".icl"
							, requestPath
							]
						)
						?None
						world
				| isError mbGrepResultLocalFuncSearchTerm
					=
						(Error
							$ errorResponse id InternalError "grep failed when searching for type definitions."
					 	, world
						)
				# {stdout} = fromOk mbGrepResultLocalFuncSearchTerm
				= (Ok $ ?Just stdout, world)
	| isError mbStdoutLocalNonTypeAnnotedFuncSearchTerm =
		(fromError mbStdoutLocalNonTypeAnnotedFuncSearchTerm, st, world)
	# (mbStdoutLocalNonTypeAnnotedFuncSearchTermSpecialCase, world) =
		// If the request was not made from a .icl or we found a type annotated function in the .icl we do not search.
		case requestMadeFromIcl
			&& mbStdoutLocalFuncSearchTerm =: (Ok (?Just ""))
			&& stdoutSpecialConstructorCase == "" of
			False = (Ok ?None, world)
			True
				# (mbGrepResultLocalFuncSearchTermSpecialCase, world)
					= callProcessWithOutput
						"grep"
						(
							[ "-P", requestFileFuncDefWithoutTypeAnnotationSearchTermSpecialCase
							, "-A", "1" // Get next line as well.
							, "-r"
							, "-n"
							, "-H"
							, "-w"
							// Only apply this search term to the .icl file from which the request is made,
							// if applicable.
							, "--include", requestFileBaseName +++ ".icl"
							, requestPath
							]
						)
						?None
						world
				| isError mbGrepResultLocalFuncSearchTermSpecialCase
					=
						(Error
							$ errorResponse id InternalError "grep failed when searching for type definitions."
					 	, world
						)
				# {stdout} = fromOk mbGrepResultLocalFuncSearchTermSpecialCase
				= (Ok $ ?Just stdout, world)
	| isError mbStdoutLocalNonTypeAnnotedFuncSearchTermSpecialCase =
		(fromError mbStdoutLocalNonTypeAnnotedFuncSearchTermSpecialCase, st, world)
	// List of lists of string containing the results, grep separates file name/line number by : and results by \n.
	// grep adds an empty newline at the end of the results which is removed by init.
	// The first two results are the file name and line number.
	// This is transformed into a list of tuples of filename and linenumber for convenience.
	# results =
		(	(\[fileName, lineNr] -> ([(fileName, toInt lineNr)])) o take 2 o split ":" <$>
				(init $ split "\n" stdoutRegSearchTerm)
		)
		++
		// - is used as a seperator for the specialConstructorCase results, as grep uses - as a group seperator.
		// when previous lines are shown instead of :.
		// In the special constructor case the previous line will be found so +1 is added to the line number.
		// Filename can contain - itself so this is accounted for.
		// The actual match is at the end so this is dropped, this is followed by the lineNr and strs that when
		// concatenated form the filename, this does not account for the match containing -.
		// So if the constructor definition itself contains hyphens in comments this breaks.
		(
			(\[lineNr:fileName] -> ([(join "-" $ reverse $ fileName, toInt lineNr + 1)]))
			o drop 1 o reverse o split "-"
			<$>
			(filterSurroundingLinesForPredUntilStopSymbol
				(flip IsMember whitespaceChars)
				[!'=', '|']
				True // Read from end to front of line.
				(init $ split "\n" stdoutSpecialConstructorCase)
			)
		)
		++
		// This case is the same as for stdoutRegSearchTerm, with a different search term.
		maybe
			[]
			(\stdoutLocalSearchTerm ->
				(	(\[fileName, lineNr] -> ([(fileName, toInt lineNr)])) o take 2 o split ":" <$>
						(init $ split "\n" stdoutLocalSearchTerm)
				)
			)
			(fromOk mbStdoutLocalSearchTerm)
		++
		maybe
			[]
			(\stdoutLocalFuncSearchTerm ->
				(	(\[fileName, lineNr] -> ([(fileName, toInt lineNr)])) o take 2 o split ":" <$>
						(init $ split "\n" stdoutLocalFuncSearchTerm)
				)
			)
			(fromOk mbStdoutLocalFuncSearchTerm)
		++
		maybe
			[]
			(\stdoutLocalNonTypeAnnotatedFuncSearchTerm ->
				(	(\[fileName, lineNr] -> ([(fileName, toInt lineNr)])) o take 2 o split ":" <$>
						(init $ split "\n" stdoutLocalNonTypeAnnotatedFuncSearchTerm)
				)
			)
			(fromOk mbStdoutLocalNonTypeAnnotedFuncSearchTerm)
		++
		// - is used as a seperator for the results, as grep uses - as a group seperator
		// when previous lines are shown instead of :.
		// Filename can contain - itself so this is accounted for.
		// the stop symbol is found is used to return the actual result line.
		// The actual match is at the end so this is dropped, this is followed by the lineNr and strs that when
		// concatenated form the filename, this does not account for the match containing -.
		// So if the constructor definition itself contains hyphens in comments this breaks.
		maybe
			[]
			(\stdoutLocalNonTypeAnnotatedFuncSearchTermSpecialCase ->
				(\([lineNr:fileName]) -> ([(join "-" $ reverse $ fileName, toInt lineNr - 1)]))
				o drop 1 o reverse o split "-"
				<$>
				(filterSurroundingLinesForPredUntilStopSymbol
					(flip IsMember alphabeticAndWhitespaceChars)
					[!'=', '|', '#']
					False // Read from front to end of line.
					(init $ split "\n" stdoutLocalNonTypeAnnotatedFuncSearchTermSpecialCase)
				)
			)
			(fromOk mbStdoutLocalNonTypeAnnotedFuncSearchTermSpecialCase)
	// For every tuple of fileName and lineNumber, a Location is generated to be sent back to the client.
	# locations = [! l \\ l <- catMaybes $ fileAndLineToLocation <$> flatten results !]
	= (locationResponse id locations, st, world)
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
		# firstUnicodeChar = fromChar $ select line charNr
		// If the first char is a space, comma, \n, or \t, go backwards
		// When a declaration is requested when a whole term is selected the character ends up being the first char
		// after the term, the same holds when attempting to go to the declaration when selecting a lookBackCharacter.
		| IsMember firstUnicodeChar lookBackCharacters = grepSearchTermFor line (UInt (charNr - 1))
		// This case is added to deal with going to the declaration of an infix function that is used prefix
		// and selecting the ( character.
		| firstUnicodeChar == lookForwardCharacter = grepSearchTermFor line (UInt (charNr + 1))
		// It should not be attempted to go the declaration of special syntax symbols.
		| isSpecialSymbol firstUnicodeChar =
			Error $
				errorResponse id ParseError "it is not possible to go to the definition of a special syntax symbol."
		// This is the general case.
		| isSymbol firstUnicodeChar || isAlphaNum firstUnicodeChar || isPunctuation firstUnicodeChar
			# stopPredicate = if (isAlphaNum firstUnicodeChar) stopPredicatePrefix stopPredicateInfixOrGenericKindSpec
			# searchTerm =
				removeUnwantedSymbolsFromSearchTerm $ retrieveSearchTerm stopPredicate line uIntChar
			# searchTerm =
				if (isInfixOf [c \\ c <-:"{|"] [c \\ c <-: searchTerm])
					searchTerm
					// If the search term does not contain a generic kind specification, we parse again using a
					// more strict predicate to avoid a problem with [(a,b):f].
					// If this is not done the search term for [(a,b):f] would become abf instead of f.
					(removeUnwantedSymbolsFromSearchTerm
						$ retrieveSearchTerm stopPredicateAfterGenericKindSpecificationWasNotFound line uIntChar
					)
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
