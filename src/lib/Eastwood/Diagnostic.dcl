definition module Eastwood.Diagnostic

from Eastwood.Range import :: Range

/**
 * Represents the warnings, errors, etc found by the passes. This type attempts to be as compatible with the LSP
 * specification as possible.
 */
:: Diagnostic =
	{ range :: !CharacterRange
	//* Where the Diagnostic was found/the subject of the diagnosic
	, severity :: !DiagnosticSeverity
	//* The severity of the diagnosic
	, dCode :: !Int
	//* An incremental number that forms a unique identifier in combination with the source. `d` because `code` is a
	//* reserved name
	, source :: !String
	//* A lowercase human readable name for the pass that created the diagnosic
	, message :: !String
	//* A human readable string describing the issue. This field should not specify the range or severity of the issue.
	//* These shall be handled by the linter or the language server.
	}

/**
 * A range as defined by the LSP specification
 */
:: CharacterRange :== Range Position

/**
 * The position as defined by the LSP specification
 */
:: Position =
	{ line :: !Int
	, character :: !Int
	}

/**
 * As defined by the LSP specification
 */
:: DiagnosticSeverity = Error | Warning | Information | Hint
