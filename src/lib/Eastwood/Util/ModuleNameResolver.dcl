definition module Eastwood.Util.ModuleNameResolver

/**
 * This module provides functionality to find the module name for a given file,
 * by scanning its header.
 */

from Data.Error import :: MaybeError
from System.FilePath import :: FilePath

/**
 * @param The path of the file to resolve the module name for.
 * @param The search paths. This is needed for a final check. For instance,
 *   A/B/C.icl with the module name `C` may be fine if A/B is a search path,
 *   but if only A is a search path we would expect the module name `B.C`.
 * @result An error when the module name could not be parsed or does not match
 *   the file path / search paths; otherwise the parsed module name.
 */
resolveModuleName :: !FilePath ![FilePath] !*World -> (!MaybeError String String, !*World)
