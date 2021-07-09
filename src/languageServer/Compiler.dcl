definition module Compiler

/**
 * This module calls `cocl` on the given file and produces Eastwood Diagnostics
 */

from Data.Error import :: MaybeError
from System.FilePath import :: FilePath
from Eastwood.Diagnostic import :: EastwoodDiagnostic

runCompiler :: !FilePath !*World -> (!MaybeError String [EastwoodDiagnostic], !*World)
