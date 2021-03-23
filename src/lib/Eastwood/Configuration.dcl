definition module Eastwood.Configuration

import Eastwood.Pass.TrailingWhitespace
import Eastwood.Range

/**
 * The configuration of all passes. The order of the passes field is respected by the library.
 */
:: Configuration =
	{ lineRanges :: ![LineRange]
	//* What lines of the given file should be considered. LineRanges are considered to be ordered and non-overlapping.
	//* The first line of a file in line 1
	, passes :: ![PassConfiguration]
	//* What passes should be performed, and what should their configuration be
	}

/**
 * Should be extened for every additional pass. Configuration of the passes should be defined in their specific module.
 */
:: PassConfiguration
	= TrailingWhitespaceConfiguration TrailingWhitespaceConfiguration
