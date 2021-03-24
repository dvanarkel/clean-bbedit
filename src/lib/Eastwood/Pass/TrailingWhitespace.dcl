definition module Eastwood.Pass.TrailingWhitespace

/**
 * This pass will only check if every line in the file doesn't end with a whitespace character and produces a diagnostic
 * for each instance.
 */

from Eastwood.Configuration import :: LineRange
from Eastwood.Diagnostic import :: Diagnostic, :: DiagnosticSeverity
from Eastwood.Range import :: Range

/**
 * The configuration for the TrailingWhitespace pass. Diagnostics are generated with the specified severity.
 */
:: TrailingWhitespaceConfiguration =
	{ severity :: !?DiagnosticSeverity
	}

/**
 * Runs the whitespace checking pass.
 */
runPass :: !TrailingWhitespaceConfiguration ![String] -> [Diagnostic]
