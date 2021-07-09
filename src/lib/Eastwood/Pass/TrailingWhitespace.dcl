definition module Eastwood.Pass.TrailingWhitespace

/**
 * This pass will only check if every line in the file doesn't end with a whitespace character and produces a diagnostic
 * for each instance.
 */

from Eastwood.Configuration import :: LineRange
from Eastwood.Diagnostic import :: EastwoodDiagnostic, :: EastwoodDiagnosticSeverity, :: DiagnosticSource
from Eastwood.Range import :: EastwoodRange

/**
 * Add our source to the list of diagnostic sources. See Eastwood.Diagnostic for more information.
 */
:: DiagnosticSource | TrailingWhitespacePass

/**
 * The configuration for the TrailingWhitespace pass. Diagnostics are generated with the specified severity.
 */
:: TrailingWhitespaceConfiguration =
	{ severity :: !?EastwoodDiagnosticSeverity
	}

/**
 * Runs the whitespace checking pass.
 */
runPass :: !TrailingWhitespaceConfiguration ![String] -> [EastwoodDiagnostic]
