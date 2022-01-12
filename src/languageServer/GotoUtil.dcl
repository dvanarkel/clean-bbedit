definition module GotoUtil

from Data.Error import :: MaybeError
from System.FilePath import :: FilePath
from Text.GenJSON import :: JSONNode
from Text.Unicode.UChar import :: UChar

from LSP.BasicTypes import :: UInt
from LSP.Internal.Serialize import generic gLSPJSONDecode
from LSP.Location import :: Location
import qualified LSP.Position
from LSP.RequestId import :: RequestId
from LSP.RequestMessage import :: RequestMessage
from LSP.ResponseMessage import :: ResponseMessage
from LSP.TextDocumentIdentifier import :: TextDocumentIdentifier

from Config import :: EastwoodState

:: GotoPrerequisites =
	{ line :: !String //* The line number from which go to request was made.
	, charNr :: !UInt //* The char number within the line from which the go to request was made.
	, rootPath :: !FilePath //* The full path leading to the Eastwood.yml file.
	, cleanHomeLibs :: ![FilePath] //* The paths to the CLEAN_HOME libraries that should be searched.
	}

:: GotoDeclarationOrDefinitionParams =
	{ textDocument :: !TextDocumentIdentifier
	, position :: !'LSP.Position'.Position
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

whitespaceChars :: [!Char]

alphabeticAndWhitespaceChars :: [!Char]

fileAndLineToLocation :: !(!String, !Int) -> ?Location

locationResponse :: !RequestId ![!Location!] -> ResponseMessage

lookBackCharacters :: [!UChar]

lookForwardCharacter :: UChar

isSpecialSymbol :: !UChar -> Bool

atleastOneWhiteSpace :: String

anyAmountOfWhitespace :: String

anyAmountOfCharacters :: String

stopPredicatePrefix :: !UChar -> Bool

stopPredicateInfixOrGenericKindSpec :: !UChar -> Bool

/**
 * Unless an infix function is being parsed this function removes the symbols surrounding the function from the string.
 * If an infix function is being parsed we remove the parenthesis surrounding the infix function if used prefix.
 *
 * @param the search term for which unwanted symbols should be removed
 * @result the search term without the unwanted symbols.
 */
removeUnwantedSymbolsFromSearchTerm :: !String -> String

isInfix :: !String -> Bool

/**
 * Parse backwards and forwards until a char that triggers the stopPredicate is found
 * or when there are no more characters to parse.
 *
 * @param The line to retrieve the search term for
 * @param The character number within the line that was selected
 * @param The resulting search term.
 */
retrieveSearchTerm :: !(UChar -> Bool) !String !UInt -> String

stopPredicateAfterGenericKindSpecificationWasNotFound :: !UChar -> Bool

grepTypeSearchTerm :: !String -> String

lineStartsWith :: String

startsWithUpper :: !String -> Bool

// Further processing has to be done for constructors that have the | or = on the previous line.
// In this case, the constructor has to be preceded by at least one whitespace only.
// For this we return a seperate search term since we have to process the previous line.
grepConstructorSearchTermSpecialCase :: !String -> ?String

//* The grep func definition search pattern is adjusted based on
//* whether an infix function or a prefix function was parsed.
grepFuncSearchTerm :: !String -> String

grepGenericSearchTerm :: !String -> String

grepClassSearchTerm :: !String -> String

grepMacroSearchTerm :: !String -> String

grepNewOrAbstractTypeSearchTerm :: !String -> String

grepTypeSynonymSearchTerm :: !String -> String

grepConstructorSearchTerm :: !String -> String
