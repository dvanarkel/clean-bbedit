definition module Compiler

from Data.Map import :: Map
from Data.Error import :: MaybeError
from System.FilePath import :: FilePath
from Eastwood.Diagnostic import :: Diagnostic

//* The compiler settings provided by the project file.
:: CompilerSettings =
	{ compilerPath :: !FilePath
		//* The full path to the `cocl` / `cocl-itasks` executable.
	, searchPaths :: ![FilePath]
		//* All search paths to find modules.
		//* These paths are assumed to be absolute, but in the project file may
		//* be specified as relative to the directory of the project file.
	}

/**
 * Calls `cocl` on the given file and produces Eastwood Diagnostics.
 *
 * @param The path to the Clean file to produce diagnostics for.
 * @param The module name of the Clean file.
 * @param The compiler settings.
 * @param The world
 * @result For each file a list of diagnostics. An entry is always present for
 *   the provided file (even if it's empty), so that the client clears the
 *   diagnostics for that file. Results for other files may be generated if
 *   they are imported by the provided file.
 *
 *   The file names contain OS path separators, i.e. `\` on Windows.
 */
runCompiler
	:: !FilePath !String !CompilerSettings !*World
	-> (!MaybeError String (Map FilePath [!Diagnostic]), !*World)
