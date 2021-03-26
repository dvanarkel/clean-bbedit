definition module Eastwood.Diagnostic

from Eastwood.Range import :: CharacterRange, :: Position, :: Range

/**
 * Represents the warnings, errors, etc found by the passes. This type attempts to be as compatible with the LSP
 * specification as possible.
 */
:: Diagnostic =
	{ range :: !CharacterRange
	//* Where the Diagnostic was found/the subject of the diagnostic
	, severity :: !DiagnosticSeverity
	//* The severity of the diagnostic
	, dCode :: !Int
	//* An incremental number that forms a unique identifier in combination with the source. `d` because `code` is a
	//* reserved name
	, source :: !DiagnosticSource
	//* A lowercase human readable name for the pass that created the diagnostic
	, message :: !String
	//* A human readable string describing the issue. This field should not specify the range or severity of the issue.
	//* These shall be handled by the linter or the language server.
	}

/**
 * As defined by the LSP specification
 */
:: DiagnosticSeverity = Error | Warning | Information | Hint

/**
 * A EADT defining all possible sources. We define this as an EADT because we don't want this module to contain
 * information about the passes. Every pass should extend this EADT with its own type, with the linter being
 * responsible for the toString instantiation.
 */
:: DiagnosticSource = ..
