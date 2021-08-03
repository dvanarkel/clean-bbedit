definition module Compiler

from Data.Map import :: Map
from Data.Error import :: MaybeError
from System.FilePath import :: FilePath
from Eastwood.Diagnostic import :: Diagnostic

/**
 * Calls `cocl` on the given file and produces Eastwood Diagnostics.
 *
 * @param The path to the Clean file to produce diagnostics for.
 * @param The folders in the workspace.
 * @param The world
 * @result For each file a list of diagnostics. Always a result is given for the provided file (even if it's empty).
 *         Results for other files can be generated if the file is the corresponding ICL/DCL of the file provided
 *         or DCLs imported by the file provided.
 */
runCompiler :: !FilePath ![!FilePath] !*World -> (!MaybeError String (Map FilePath [!Diagnostic]), !*World)
