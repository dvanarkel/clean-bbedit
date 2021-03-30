definition module Eastwood.Range

/**
 * This module contains all types and utility functions that deal with ranges of any kind.
 */

/**
 * A parameterized range with a lower and an upper bound (inclusive).
 *
 * This type can be instantiated. E.g. for the LSP protocol, this range is
 * instantiated with Position; for the configuration with optional line numbers.
 *
 * The first line of a file is line 1; the first column of a line is column 1.
 */
:: Range t =
	{ start :: !t
	, end :: !t
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
 * From where to where should passes analyze the code. A ?None indicates that that side of the range is unbounded (i.e.
 * the start or the end of the entire file).
 */
:: LineRange :== Range (?Int)

/**
 * Creates a character range for the characters on a single line.
 *
 * @param The line number
 * @param The start column number
 * @param The end column number
 * @result The range
 */
singleLineRange :: !Int !Int !Int -> CharacterRange

/**
 * Checks if the provided line range partially encapsulates the character range. Handles ?None bounds.
 *
 * @param The line range
 * @param The character range
 * @result True if the number falls partially or fully inside of the range, False otherwise
 */
inLineRange :: !LineRange !CharacterRange -> Bool

/**
 * Checks if the provided character range falls entirely beyond the provided line range. Handles ?None bounds.
 *
 * @param The line range
 * @param The character range
 * @result True if the line number is greater than the end bound of the line range, False otherwise
 */
afterLineRange :: !LineRange !CharacterRange -> Bool
