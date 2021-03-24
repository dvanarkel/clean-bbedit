definition module Eastwood.Range

/**
 * This module contains all types and utility functions that deal with ranges of any kind.
 */

/**
 * A parameterized range with a lower an upperbound. Can be instantiated, e.g. for the LSP protocol, this range is
 * instantiated with Position, for the configuration with the line numbers.
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
 * Checks if the provided line range contains the provided line number. Handles ?None bounds.
 *
 * @param The line range
 * @param The line number
 * @result True if the number falls inside of the range, False otherwise
 */
inLineRange :: !LineRange !Int -> Bool

/**
 * Checks if the provided line number falls beyond the provided line range. Handles ?None bounds.
 *
 * @param The line range
 * @param The line number
 * @result True if the line number is greater than the end bound of the line range, False otherwise
 */
afterLineRange :: !LineRange !Int -> Bool
