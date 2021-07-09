definition module Eastwood

/**
 * Eastwood is a library that can be used to analyze Clean source code and is intended for use in a linter and language
 * server.
 */

from Data.Error import :: MaybeError
from System.File import :: FileError
from System.FilePath import :: FilePath

from Eastwood.Configuration import :: Configuration
from Eastwood.Diagnostic import :: EastwoodDiagnostic

/**
 * Runs Eastwood on the contents of the specific FilePath.
 *
 * @param The Eastwood configuration
 * @param The file for which Eastwood should generate diagnostics
 * @param The world
 * @result The Diagnostics or an error string in case reading the file went wrong
 * @result The world
 */
runPassesFile :: !Configuration !FilePath !*World -> (MaybeError String [EastwoodDiagnostic], !*World)

/**
 * Runs Eastwood on the given Clean code.
 *
 * This function only runs passes for which the Clean compiler is not needed
 * (because the compiler needs to read from a file). Other passes are silently
 * ignored.
 *
 * @param The Eastwood configuration
 * @param The piece of Clean code for which Eastwood should generate Diagnostics
 * @result The Diagnostics
 */
runPassesString :: !Configuration !String -> [EastwoodDiagnostic]
