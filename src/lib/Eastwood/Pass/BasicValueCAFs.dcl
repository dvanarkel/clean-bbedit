definition module Eastwood.Pass.BasicValueCAFs

/**
 * This pass checks for CAFs with constant basic values, like `x =: 5`. These are useless.
 */

from Clean.Parse import :: HashTable, :: Module, :: ParsedDefinition, :: ParsedModule

from Eastwood.Diagnostic import :: EastwoodDiagnostic, :: EastwoodDiagnosticSeverity, :: DiagnosticSource

/**
 * Add our source to the list of diagnostic sources. See Eastwood.Diagnostic for more information.
 */
:: DiagnosticSource | BasicValueCAFsPass

/**
 * The configuration for the SyntaxChoices pass.
 */
:: BasicValueCAFsConfiguration =
	{ severity :: !?EastwoodDiagnosticSeverity //* The severity to generate diagnostics with.
	}

/**
 * Runs the syntax choices pass.
 */
runPass :: !BasicValueCAFsConfiguration ![String] !ParsedModule HashTable -> [EastwoodDiagnostic]
