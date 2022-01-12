definition module GotoUtil

from Data.Error import :: MaybeError
from System.FilePath import :: FilePath
from Text.GenJSON import :: JSONNode
from Text.Unicode.UChar import :: UChar

from LSP.BasicTypes import :: UInt
from LSP.Internal.Serialize import generic gLSPJSONDecode
from LSP.Location import :: Location
from LSP.Position import :: Position
from LSP.RequestId import :: RequestId
from LSP.RequestMessage import :: RequestMessage
from LSP.ResponseMessage import :: ResponseMessage
from LSP.TextDocumentIdentifier import :: TextDocumentIdentifier

from Config import :: EastwoodState

//* Context information for a request concerning a specific line and character.
:: GotoPrerequisites =
	{ line :: !String //* The line number from which go to request was made.
	, charNr :: !UInt //* The char number within the line from which the go to request was made.
	, rootPath :: !FilePath //* The full path leading to the Eastwood.yml file.
	, cleanHomeLibs :: ![FilePath] //* The paths to the CLEAN_HOME libraries that should be searched.
	}

//* Parameters for goto declaration or goto definition requests.
:: GotoDeclarationOrDefinitionParams =
	{ textDocument :: !TextDocumentIdentifier //* The document.
	, position :: !Position //* The position within the document.
	}

derive gLSPJSONDecode GotoDeclarationOrDefinitionParams

/**
 * Retrieves the prerequisites for being able to go to definitions/declarations.
 *
 * @param The go to definition/declaration request
 * @param The eastwood state
 * @param World
 * @result ResponseMessage in case of error, the prerequisites in case of success
 * @result World
 */
gotoPrerequisitesFor
	:: !RequestMessage !EastwoodState !*World -> (MaybeError ResponseMessage GotoPrerequisites, !*World)

/**
 * This function is used to check the surrounding lines included in a grep result ending on a provided symbol
 * (possibly followed by whitespace)
 *
 * @param The predicate that should hold for the characters that are found before finding a stop symbol.
 * @param The stop symbols which should lead to short circuiting as the symbol which is searched for is found.
 * @param Whether the line should be reversed (check for stop symbol reading from end of line to front)
 * @param The lines to check
 * @result The resulting lines that pass the filter.
 */
filterSurroundingLinesForPredUntilStopSymbol :: !(Char -> Bool) ![!Char] !Bool ![String] -> [String]

//* A list of whitespace characters.
whitespaceChars :: [!Char]

//* A list of alphabetic and whitespace characters.
alphabeticAndWhitespaceChars :: [!Char]

/**
 * Converts a file and a line number to a location.
 * @param A tuple of a file path and a line number (1-based).
 * @result A location, or ?None when the file doesn't exist.
 */
fileAndLineToLocation :: !(!String, !Int) -> ?Location

/**
 * Wraps a request ID and a list of locations in a response message.
 * @param The request ID.
 * @param The list of locations.
 * @result A response message.
 */
locationResponse :: !RequestId ![!Location!] -> ResponseMessage

//* Not all characters belong to a searchable symbol. If a goto request is received for one of these "look-back"
//* characters, the parser scans the file backward from that position until it finds an actual symbol.
lookBackCharacters :: [!UChar]

//* Not all characters belong to a searchable symbol. If a goto request is received for one of these "look-forward"
//* characters, the parser scans the file forward from that position until it finds an actual symbol.
lookForwardCharacter :: UChar

//* Not all characters belong to a searchable symbol. If a goto request is received for one of these "special"
//* characters, Eastwood will not attempt to search.
//* {, } are not included because they can occur in a generic kind specification e.g: {|*|}
isSpecialCharacter :: !UChar -> Bool

//* A regex matching at least one white space.
atleastOneWhiteSpace :: String

//* A regex matching any amount of white space.
anyAmountOfWhitespace :: String

//* A regex matching any amount of characters.
anyAmountOfCharacters :: String

//* A regex matching the start of a line.
lineStartsWith :: String

/** A predicate that indicates when to stop parsing.
 * @param The character.
 * @result The predicate.
 */
stopPredicate :: !UChar -> (UChar -> Bool)

//* A predicate that indicates when to stop parsing, that also includes symbols used within the generic kind
//* specification syntax.
stopPredicateAfterGenericKindSpecificationWasNotFound :: !UChar -> Bool

/**
 * Unless an infix function is being parsed this function removes the symbols surrounding the function from the string.
 * If an infix function is being parsed we remove the parenthesis surrounding the infix function if used prefix.
 *
 * @param the search term for which unwanted symbols should be removed
 * @result the search term without the unwanted symbols.
 */
removeUnwantedSymbolsFromSearchTerm :: !String -> String

/**
 * Indicates if the search term is an infix symbol.
 * @param The search term.
 * @result A boolean indicating whether the search term is an infix symbol.
 */
isInfix :: !String -> Bool

/**
 * Parse backwards and forwards until a char that triggers the stopPredicate is found
 * or when there are no more characters to parse.
 * @param The stop predicate.
 * @param The line to retrieve the search term for.
 * @param The character number within the line that was selected.
 * @result The resulting search term.
 */
retrieveSearchTerm :: !(UChar -> Bool) !String !UInt -> String

/**
 * Indicates if a string starts with an upper case letter.
 * @param The string.
 * @result A boolean indicating whether the string starts with an upper case letter.
 */
startsWithUpper :: !String -> Bool

/** A grep search term for types.
 * @param The raw seach term.
 * @result The grep search term.
 */
grepTypeSearchTerm :: !String -> String

/** A grep search term for functions.
 * @param The raw seach term.
 * @result The grep search term.
 */
grepFuncSearchTerm :: !String -> String

/** A grep search term for generics.
 * @param The raw seach term.
 * @result The grep search term.
 */
grepGenericSearchTerm :: !String -> String

/** A grep search term for classes.
 * @param The raw seach term.
 * @result The grep search term.
 */
grepClassSearchTerm :: !String -> String

/** A grep search term for macros.
 * @param The raw seach term.
 * @result The grep search term.
 */
grepMacroSearchTerm :: !String -> String

/** A grep search term for new or abstract types.
 * @param The raw seach term.
 * @result The grep search term.
 */
grepNewOrAbstractTypeSearchTerm :: !String -> String

/** A grep search term for type synonyms.
 * @param The raw seach term.
 * @result The grep search term.
 */
grepTypeSynonymSearchTerm :: !String -> String

/** A grep search term for constructors.
 * @param The raw seach term.
 * @result The grep search term.
 */
grepConstructorSearchTerm :: !String -> String

/** A special case grep search term for constructors that have the | or = on the previous line.
 * @param The raw seach term.
 * @result The grep search term.
 */
grepConstructorSearchTermSpecialCase :: !String -> ?String
