implementation module GotoDeclaration

import StdEnv
import StdOverloadedList

import Data.Array
import Data.Bool
import Data.Error
import Data.Func
import Data.Functor
import Data.List
import Data.Maybe
import Data.Tuple
import Text
import Text.Encodings.UrlEncoding
from Text.Unicode.UChar import isSymbol, :: UChar, instance fromChar UChar, instance toChar UChar, instance == UChar
	, instance toInt UChar, isAlphaNum, isPunctuation
import Text.URI
import System.Environment
import System.File
import System.FilePath
from System.Process import :: ProcessResult {..}, callProcessWithOutput

import LSP.BasicTypes
import LSP.Location
import LSP.RequestMessage
import LSP.ResponseMessage
import LSP.Internal.Serialize
import LSP.TextDocumentIdentifier
import qualified LSP.Position
from LSP.Position import :: Position {..}
import qualified Eastwood.Range
from LSP.Range import :: Range {..}
from Eastwood.Range import qualified :: Range {..}, :: CharacterRange, :: Position {..}
from LSP.Position import qualified :: Position {..}
import qualified LSP.Range

import Eastwood.Util.FileFinder

import Config
import Constants
import GotoUtil
import Util

onGotoDeclaration
    :: !RequestMessage !EastwoodState !*World -> (!ResponseMessage, !*World)
onGotoDeclaration req=:{RequestMessage|id} st world
	# (mbPrerequisites, world) = gotoPrerequisitesFor req st world
	| isError mbPrerequisites = (fromError mbPrerequisites, world)
    # {line, charNr, rootPath, cleanHomeLibs} = fromOk mbPrerequisites
	# mbSearchTerms = grepSearchTermFor line charNr
	| isError mbSearchTerms = (fromError mbSearchTerms, world)
	// Using the searchString, grep is executed to find the file names and line numbers of the definitions that match.
	// There is a special case for constructors which have the preceding | or = on the preceding line.
	// This requires checking the previous line, hence there is a seperate search term for efficiency reasons.
	# (searchTermReg, searchTermConstructorSpecialCase)  = fromOk mbSearchTerms
	// Call grep using the regular (non special constructor case) search term to find the matching declarations.
	// -P enables perl regexp, -r recurses through all files, -n gives line number,
	// -w matches whole words only (e.g: :: Maybe matches :: Maybe but not :: MaybeOSError).
	//--include \*.dcl makes sure only dcl files are examined by grep.
	// rootPath and cleanHomeLibs are searched for matches.
	# (mbGrepResultRegSearchTerm, world)
		= callProcessWithOutput
			"grep"
			(
				[ "-P"
				, searchTermReg
				, "-r"
				, "-n"
				, "-w"
				, "--include"
				, "\*.dcl"
				, rootPath
				]
				++ cleanHomeLibs
			)
			?None
			world
	| isError mbGrepResultRegSearchTerm
		= (errorResponse id InternalError "grep failed when searching for type definitions.", world)
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
						([ "-P", searchTerm
						// -B 1 also selects the previous line.
						, "-B", "1"
						, "-r"
						, "-n"
						, "-w"
						, "--include", "\*.dcl"
						, rootPath
						]
						++ cleanHomeLibs
						)
					?None
					world
	| isJust mbGrepResultSpecialConstructorCase && (isError $ fromJust mbGrepResultSpecialConstructorCase)
		= (errorResponse id InternalError "grep failed when searching for special constructor case.", world)
	# stdoutSpecialConstructorCase = case mbGrepResultSpecialConstructorCase of
		// No need to search for a constructor since the searchTerm can not be a constructor.
		?None = ""
		// There is a need to search for a constructor since the searchTerm could be a constructor.
		?Just grepResult = (fromOk grepResult).ProcessResult.stdout
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
		(	(\[lineNr:fileName] -> ([(join "-" $ reverse $ fileName, toInt lineNr + 1)])) o drop 1 o reverse o split "-"
			<$>
			(filterSurroundingLinesForPredUntilStopSymbol
				(flip IsMember whitespaceChars)
				[!'=', '|']
				True
				(init $ split "\n" stdoutSpecialConstructorCase)
			)
		)
	// For every tuple of fileName and lineNumber, a Location is generated to be sent back to the client.
	# locations = [! l \\ l <- catMaybes $ fileAndLineToLocation <$> flatten results !]
	= (locationResponse id locations, world)
where
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
				if (isInfixOf [c \\ c <-:"{|"] [c \\ c <-: searchTerm] || not (isAlphaNum firstUnicodeChar))
					searchTerm
					// If the search term does not contain a generic kind specification, we parse again using a
					// more strict predicate to avoid a problem with [(a,b):f].
					// If this is not done the search term for [(a,b):f] would become abf instead of f.
					(removeUnwantedSymbolsFromSearchTerm
						$ retrieveSearchTerm stopPredicateAfterGenericKindSpecificationWasNotFound line uIntChar
					)
			= Ok $
				(concat
					// Only search for types when the term starts with an uppercase character.
					[ if (grepTypeSearchTerm searchTerm == "") "" (grepTypeSearchTerm searchTerm +++ "|")
					, grepFuncSearchTerm searchTerm
					, "|"
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
				, grepConstructorSearchTermSpecialCase searchTerm)
		= Error $
			errorResponse
			id
			ParseError
			("Unrecognised char with unicode : " +++ (toString $ toInt firstUnicodeChar))
