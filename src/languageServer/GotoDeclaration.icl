implementation module GotoDeclaration

import Config
import Constants

import Data.Array
import Data.Bool
import Data.Error
import qualified Data.Foldable
import Data.Func
import Data.Functor
import Data.List
import Data.Maybe
import Data.Tuple

import qualified Eastwood.Range
from Eastwood.Range import qualified :: Range {..}, :: CharacterRange, :: Position {..}
import Eastwood.Util.FileFinder

import GotoUtil

import Text
import Text.Encodings.UrlEncoding
from Text.Unicode.UChar import isSymbol, :: UChar, instance fromChar UChar, instance toChar UChar, instance == UChar
	, instance toInt UChar, isAlphaNum, isPunctuation
import Text.URI

import StdEnv
import StdOverloadedList

import System.Environment
import System.File
import System.FilePath
from System.Process import :: ProcessResult {..}, callProcessWithOutput

import LSP.BasicTypes
import LSP.Internal.Serialize
import LSP.Location
import qualified LSP.Position
from LSP.Position import :: Position {..}
from LSP.Position import qualified :: Position {..}
import qualified LSP.Range
from LSP.Range import :: Range {..}
import LSP.RequestMessage
import LSP.ResponseMessage
import LSP.TextDocumentIdentifier
import Util

onGotoDeclaration
    :: !RequestMessage !EastwoodState !*World -> (!ResponseMessage, !*World)
onGotoDeclaration req=:{RequestMessage|id} st world
	# (mbPrerequisites, world) = gotoPrerequisitesFor req st world
	| isError mbPrerequisites = (fromError mbPrerequisites, world)
    # {line, charNr, rootPath, cleanHomeLibs} = fromOk mbPrerequisites
	# mbSearchTerms = grepSearchTermFor line charNr
	| isError mbSearchTerms = (fromError mbSearchTerms, world)
	// Using the searchString, grep is executed to find the file names and line numbers of the declarations that match.
	# (searchTermReg, searchTermConstructorSpecialCase)  = fromOk mbSearchTerms
	# (mbResultsGeneralCase, world) =
		grepResultsForSearchTerm
			Declaration ?None searchTermReg rootPath cleanHomeLibs [] singleLineGrepStdoutToFilePathAndLineNr id world
	| isError mbResultsGeneralCase = (fromError mbResultsGeneralCase, world)
	// There is a special case for constructors which have the preceding | or = on the preceding line.
	// This requires checking the previous line, hence there is a seperate search term.
	# (mbResultsCtorWithPipeOrEqOnPrevLine, world) =
		'Data.Foldable'.foldl`
			(\(_, world) searchTerm ->
				grepResultsForSearchTerm Declaration ?None searchTerm rootPath cleanHomeLibs ["-B", "1"]
					(surroundingLineGrepStdoutToFilePathAndLineNr
						(flip IsMember whitespaceChars)
						[!'=', '|']
						True
						1
					)
					id world
			)
			(Ok [], world)
			searchTermConstructorSpecialCase
	| isError mbResultsCtorWithPipeOrEqOnPrevLine = (fromError mbResultsCtorWithPipeOrEqOnPrevLine, world)
	# results = fromOk mbResultsGeneralCase ++ fromOk mbResultsCtorWithPipeOrEqOnPrevLine
	// For every tuple of fileName and lineNumber, a Location is generated to be sent back to the client.
	# locations = [! l \\ l <- catMaybes $ fileAndLineToLocation <$> results !]
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
			# searchTerm = escapeRegexCharactersInSearchTerm searchTerm
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
