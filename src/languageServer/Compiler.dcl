definition module Compiler

from Data.Error import :: MaybeError
from System.FilePath import :: FilePath
from Eastwood.Diagnostic import :: EastwoodDiagnostic

/**
 * Calls `cocl` on the given file and produces Eastwood Diagnostics
 */
runCompiler :: !FilePath !*World -> (!MaybeError String [EastwoodDiagnostic], !*World)
